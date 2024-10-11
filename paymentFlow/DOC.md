### paymentFlow plugin

#### What it does?

`paymentFlow` is a compiler plugin designed to incorporate business logic validation checks during compilation. It performs the following verification:

***Restrict Access to Specified Type Fields***: This check ensures that deprecated fields within a type are not accessed. The goal is to prevent usage of these restricted fields and to suggest alternative methods for accessing the required information.