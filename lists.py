from talon import Module, Context

mod = Module()

ctx = Context()
ctx.matches = "app: /organteq/i"

mod.list("organteq_manual", desc="Organteq manuals from bottom to top, numbers 1 through 4")
ctx.lists["self.organteq_manual"] = {
    "pedal": "1",
    "choir": "2",
    "core": " 2",
    "great": "3",
    "swell": "4",
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

