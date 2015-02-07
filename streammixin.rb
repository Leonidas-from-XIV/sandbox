class Stream
  def read
    puts 'Reading'
    return 0
  end
  def write(t)
    puts "Writing #{t}"
  end
end

class FStream < Stream
  attr_accessor :fname
  def initialize
    @fname = 'myfile'
  end
  def read
    data = super
    puts "From #{@fname}"
    return data
  end
  def write(p)
    super(p)
    puts "To #{@fname}"
  end
end

module SynchRW
  def acquireLock
    puts 'Lock acquired'
  end
  def releaseLock
    puts 'Lock released'
  end
  def write(p)
    acquireLock
    super(p)
    releaseLock
  end
  def read
    acquireLock
    data = super
    releaseLock
    return data
  end
end

class SynchedFStream < FStream
  include SynchRW
end

s = SynchedFStream.new
s.write('Important data')
s.read
