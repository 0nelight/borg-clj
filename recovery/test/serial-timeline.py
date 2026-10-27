#!/usr/bin/env python3
"""Starts QEMU with the serial console on stdio and writes down every line with
the seconds since power-on.

Why the serial console and not the screen: firmware (OVMF), GRUB, kernel,
live-boot and systemd all write there once console=ttyS0 is on the kernel
command line. A timeline of it shows where a boot stands -- and for how long.
Silence of more than five seconds is noted on its own line, because that is
exactly what looks like a hang on real hardware (GRUB shows no progress while
loading).

  serial-timeline.py [--stop REGEX] [--linger S] [--shot FILE] [--hold S]
                     LOG MAXSECONDS -- QEMU-ARGUMENTS...

--stop    pattern that marks the run as successful
--linger  keep running this many seconds after it
--shot    a screenshot at the end (PPM, over QMP) -- shows what is on tty1
--hold    keep the VM running this many seconds afterwards (to look around)
"""
import argparse
import json
import os
import re
import select
import socket
import subprocess
import sys
import time

p = argparse.ArgumentParser()
p.add_argument('--stop', default=r'Spawning shell|login:')
p.add_argument('--linger', type=float, default=0)
p.add_argument('--shot')
p.add_argument('--hold', type=float, default=0)
p.add_argument('log')
p.add_argument('maxs', type=float)
if '--' not in sys.argv:
    p.error('QEMU arguments missing (after --)')
cut = sys.argv.index('--')
a = p.parse_args(sys.argv[1:cut])
qargs = sys.argv[cut + 1:]

stop = re.compile(a.stop)
qmp = a.log + '.qmp'
if a.shot:
    qargs += ['-qmp', f'unix:{qmp},server=on,wait=off']
q = subprocess.Popen(['qemu-system-x86_64', '-nographic', *qargs],
                     stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                     stderr=subprocess.STDOUT)
t0 = last = time.time()
out = open(a.log, 'w')
ansi = re.compile(r'\x1b\[[0-9;?]*[A-Za-z]')


def emit(line):
    global last
    line = ''.join(c for c in line if c.isprintable()).strip()
    if not line:
        return
    now = time.time()
    if now - last > 5:
        out.write(f'        ... {now - last:.0f}s no output\n')
    out.write(f'{now - t0:7.1f}s  {line[:200]}\n')
    out.flush()
    last = now


buf = b''
done = False
stop_at = None


def pump(until):
    """Reads the serial console until until() is true or QEMU ends."""
    global buf, done, stop_at
    while q.poll() is None and not until():
        r, _, _ = select.select([q.stdout], [], [], 1)
        if not r:
            continue
        chunk = os.read(q.stdout.fileno(), 65536)
        if not chunk:
            break
        # HttpBootDxe writes its percentage into one single line
        buf += chunk.replace(b'\r', b'\n').replace(b'  Downloading', b'\nDownloading')
        *lines, buf = buf.split(b'\n')
        for raw in lines:
            s = ansi.sub('', raw.decode('latin1'))
            m = re.match(r'Downloading\.\.\.(\d+)%', s.strip())
            if m and m.group(1) not in ('1', '50', '100'):
                continue
            emit(s)
            if not done and stop.search(s):
                emit('== TARGET REACHED ==')
                done = True
                stop_at = time.time() + a.linger


pump(lambda: time.time() - t0 >= a.maxs or (done and time.time() >= stop_at))

if not done:
    emit('== GAVE UP: time limit ==' if q.poll() is None
         else f'== QEMU ended ({q.returncode}) ==')
if a.shot and q.poll() is None:
    s = socket.socket(socket.AF_UNIX)
    s.connect(qmp)
    f = s.makefile('rw')
    f.readline()
    for c in ({'execute': 'qmp_capabilities'},
              {'execute': 'screendump', 'arguments': {'filename': a.shot}}):
        f.write(json.dumps(c) + '\n')
        f.flush()
        while not any(k in json.loads(f.readline()) for k in ('return', 'error')):
            pass
    emit(f'screenshot: {a.shot}')
if a.hold and q.poll() is None:
    emit(f'VM keeps running for {a.hold:.0f}s')
    end = time.time() + a.hold
    pump(lambda: time.time() >= end)
q.kill()
if os.path.exists(qmp):
    os.remove(qmp)
sys.exit(0 if done else 1)
