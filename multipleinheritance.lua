function createClass(...)
	-- store the varargs in some odd thing
	local parents = {...}
	local c = {}
	setmetatable(c, {
		__index = function (t, k)
			for _, parent in pairs(parents) do
				local v = parent[k]
				if v then return v end
			end
			return nil
		end
	})
	c.__index = c
	function c:new(o)
		local o = o or {}
		setmetatable(o, c)
		return o
	end
	return c
end

Doctor = { prefix="Dr. " }
Researcher = { postfix=" , Ph.D." }
Honored = { honor=" summa cum laude" }

HonoredResearchingDoctor = createClass(Doctor, Researcher, Honored)
axel = HonoredResearchingDoctor:new({name="Axel Simon"})
print(axel.prefix..axel.name..axel.postfix..axel.honor)
