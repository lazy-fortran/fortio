import subprocess
import sys


executable, output = sys.argv[1:]
child = subprocess.run([executable, output], check=False)
if child.returncode == 0:
    raise SystemExit("abort fixture unexpectedly returned success")

dump = subprocess.run(["h5dump", "-n", output], check=False)
if dump.returncode != 0:
    raise SystemExit("last committed streaming image is not readable HDF5")
