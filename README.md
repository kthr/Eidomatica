Installation
--------------
First clone the repository with
```bash
git clone https://github.com/kthr/Eidomatica.git
```
Then create a directory "build" inside the cloned repository
```bash
cd Eidomatica
mkdir build
cd build
```
Finally install the library to your local Mathematica applications directory 
with
```bash
cmake ..
make install
```

Additional information
--------------
In order to use all functionality of this package you should consider installing [libEidomatica](https://github.com/kthr/libEidomatica). This library implements a some algorithms in C++ and makes them available to Mathematica.
