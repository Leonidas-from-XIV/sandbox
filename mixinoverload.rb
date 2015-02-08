module M
  def f
    puts "Hi, I'm M.f"
  end
end

module N
  def f
    puts "Hi, I'm N.f"
  end
end

class C
  include M
  include N
end

c = C.new
c.f
