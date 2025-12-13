from talon import Module, Context

mod = Module()

ctx = Context()
ctx.matches = "app: /organteq/i"

mod.list("organteq_manual", desc="Organteq manuals from bottom to top, numbers 1 through 4")
ctx.lists["self.organteq_manual"] = {
	"pedal",
	"choir",
	"great",
	"swell",
}

mod.list("organteq_stop_number", desc="Organteq stop numbers from one to twenty")
ctx.lists["self.organteq_stop_number"] = {
	"one": "1",
	"two": "2",
	"three": "3",
	"four": "4",
	"five": "5",
	"six": "6",
	"seven": "7",
	"eight": "8",
	"nine": "9",
	"ten": "10",
	"eleven": "11",
	"twelve": "12",
	"thirteen": "13",
	"fourteen": "14",
	"fifteen": "15",
	"sixteen": "16",
	"seventeen": "17",
	"eighteen": "18",
	"nineteen": "19",
	"twenty": "20",
}

mod.list("organteq_tonal_family", desc="A list of standard organ tonal families (plural forms as spoken)")
ctx.lists["self.organteq_tonal_family"] = {
	"principals": "principal",
	"principles": "principal",
	"flutes": "flute",
	"strings": "string",
	"reeds": "reed",
	"mutations": "mutation",
	"mixtures": "mixture",
}

mod.list("organteq_footage", desc="Standard organ stop footages")
ctx.lists["self.organteq_footage"] = {
	"thirty two foot": "32",
	"sixteen foot": "16",
	"eight foot": "8",
	"four foot": "4",
	"two foot": "2",
	"one foot": "1",
}

mod.list("organteq_mono_coupler", desc="Mono coupler names")
ctx.lists["self.organteq_mono_coupler"] = {
	"melody one": "melody 1",
	"melody two": "melody 2",
	"bass one": "bass 1",
	"bass two": "bass 2",
}

