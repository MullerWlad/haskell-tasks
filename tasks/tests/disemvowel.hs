type Message = String
type Template = String 

template :: Template
template = "AaEeIiOoUuYy"

result :: Message -> Message
result = filter (not . (`elem` template))