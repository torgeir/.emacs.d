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

console.log(42)

var asdf = () => +"a"
var a: number = asdf()

console.log(a)
console.log(torgeir.address.city)
