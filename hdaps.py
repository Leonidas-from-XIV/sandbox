#!/bin/env python
"""
	vim:fileencoding=iso-8859-1:ft=python
	
	Yet another HDAPS class
	Copyright (c) Phillip Berndt, 2006

	Call this file for some examples.
"""
import time

class HDAPS:
	"""
		A basic class for HDAPS access
	"""
	__calibration = ()
	HIT_RIGHT = -1
	HIT_LEFT  = 1

	def __init__(self):
		self.calibrate()

	def __valueGet(self, file):
		"""
			Load a rotation tuple from HDAPS
		"""
		try:
			valueTuple = open("/sys/bus/platform/devices/hdaps/%s" % file, "r").read()[1:-2]
			return map(lambda x: int(x), valueTuple.split(","))
		except:
			raise "HDAPS module not loaded or HDAPS not working"

	def calibrate(self):
		"""
			Calibrate the object using the HDAPS calibration
		"""
		self.__calibration = self.__valueGet("calibrate")

	def getAbsolute(self):
		"""
			Get absolute position
		"""
		return self.__valueGet("position")
	
	def getRelative(self, round = 1):
		"""
			Get relative position

			The parameter round is for rounding values
			(p.e. round = 5 means every value x returned will be
			 x mod 5 = 0)
		"""
		position = [0, 0]
		while max(position) is 0:
			position = self.getAbsolute()
		relative = [ int(float(self.__calibration[n] - position[n]) / round) * round for n in (0, 1) ]
		return relative

	def getShock(self, sens = 8):
		"""
			Wait until a shock in the X-values is noticed
			(e.g. someone hit the notebook from the side)

			Sens means sensitivity. Higher values means less
			false-positives and more true-negatives.

			This function will ignore anything if the laptop
			if moved too much
		"""
		lastValues = []
		while len(lastValues) < 16:
			time.sleep(0.005)
			lastValues.append(self.getRelative(5)[0])
		while 1:
			time.sleep(0.005)
			lastValues.append(self.getRelative(5)[0])
			if len(lastValues) > 16:
				lastValues.pop(0)
			if max(lastValues[8:]) > sens and min(lastValues[8:]) < -sens:
				if (sum(map(lambda x: abs(x), lastValues[:8])) / 8) > 10:
					continue
				for value in lastValues[8:]:
					if value is not 0:
						return value / abs(value)

class Smack:
	"""
		A smack class (for smackbook-like applications)
		Override this one ;)
	"""
	myHDAPS = None

	def __init__(self):
		self.myHDAPS = HDAPS()

	def loop(self):
		"""
			Loop and wait for shocks
		"""
		while 1:
			shock = self.myHDAPS.getShock()
			if shock is self.myHDAPS.HIT_RIGHT:
				self.hit_right()
			elif shock is self.myHDAPS.HIT_LEFT:
				self.hit_left()
	
	def hit_right(self):
		"""
			This function is called when the laptop
			is hit on the right side
		"""
		print "TODO: Implement hit_right()"
	
	def hit_left(self):
		"""
			This function is called when the laptop
			is hit on the left side
		"""
		print "TODO: Implement hit_left()"

def test():
	"""
		Test HDAPS. Shows every change in the
		HDAPS values
	"""
	myHDAPS = HDAPS()
	last  = 0
	start = time.clock()
	while 1:
		new = myHDAPS.getRelative(4)[0]
		if new == last:
			continue
		last = new
		print "[%f] %d" % (time.clock() - start, last)
		time.sleep(0.01)

if __name__ == "__main__":
	import os, math, sys

	class Smack_ChangeDesktop(Smack):
		"""
			A demo for the smack-class
			(My laptop has special keys, F19 and F20, for desktop change)
		"""
		def hit_right(self):
			pipe = os.popen("xmacroplay :0 &>/dev/null", "w")
			pipe.write("KeyStr F19")
			pipe.close()
			
		def hit_left(self):
			pipe = os.popen("xmacroplay :0 &>/dev/null", "w")
			pipe.write("KeyStr F20")
			pipe.close()

	class Smack_Lock(Smack):
		"""
			Another demo: Lock screen and unlock after 2 smacks left and
			1 smack right
		"""
		time  = 0
		stage = 0
		xlock = 0

		def __init__(self):
			self.xlock = os.spawnvp(os.P_NOWAIT, "xlock", [ "xlock", "-mode", "blank" ])
			Smack.__init__(self)

		def hit_left(self):
			if time.clock() - self.time > 2:
				self.time = time.clock()
				self.stage = 0
			if self.stage is 0:
				self.stage = 1
			elif self.stage is 1:
				self.stage = 2
			elif self.stage is 2:
				pass
			else:
				self.stage = 0
		def hit_right(self):
			if time.clock() - self.time > 2:
				self.time = time.clock()
				self.stage = 0
			if self.stage is 2:
				print "Magic hit-sequence..."
				os.kill(self.xlock, 2) # Int
				time.sleep(1)
				os.kill(self.xlock, 9) # Kill
				sys.exit(0)
			self.stage = 0

	class Smack_ChangeVT(Smack):
		"""
			A third demo for the smack-class
			Change VT
		"""
		vt = 1

		def hit_right(self):
			self.vt -= 1
			os.system("chvt %d" % self.vt)
			
		def hit_left(self):
			self.vt += 1
			os.system("chvt %d" % self.vt)

	print "Smackbook demo"
	print "A demonstration for this python interface"
	print
	print "Demo 1: Start xlock and kill it upon 2x smack left followed by 1x smack right (requires xlock)"
	print "Demo 2: Call xmacroplay upon smacks to send F19 and F20 (Change desktop, needs to be configured in your WM!)"
	print "Demo 3: chvt on smack (Requires root permissions on most systems)"
	print "Demo 4: Print events"
	print
	print "Please enter your choice: ",
	cmd = 0
	while cmd not in range(1, 5):
		cmd = input()
	if cmd is 1:
		mySmack = Smack_Lock()
	elif cmd is 2:
		mySmack = Smack_ChangeDesktop()
	elif cmd is 3:
		mySmack = Smack_ChangeVT()
	elif cmd is 4:
		mySmack = Smack()
	mySmack.loop()
