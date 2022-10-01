// example
type Name = string
type City = string
type StreetNumber = string
type Street = { no: StreetNumber }
type Address = { city: City; street: Street }
type Person = { name: Name; address: Address }

var torgeir: Person = {
  name: "Torgeir",
  address: {
    city: "Trondheim",
    street: {
      no: "4",
    },
  },
}

console.log(123)
console.log(123)

function asdf() {
  return 1
}

asdf()
