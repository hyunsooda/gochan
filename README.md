# gochan

gochan is a library that offers thread communication methods, drawing significant inspiration from the principles of Golang.

### Features
- Strong typing at compile time
    - Recevable channel (`<-chan`): Receive opration is only available.
    - Sendable channel  (`chan<-`): Send opration is only available.
    - Bidirectional channel (`chan`): Both operations are available.
