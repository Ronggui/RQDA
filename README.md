# RQDA

[RQDA](http://rqda.r-forge.r-project.org/) is computer-aided qualitative data analysis package

## Installation of Devel Version

Assuming you have a working version of current release (and all dependencies).

```R
install.packages("devtools") ## install it only if you haven't done it yet
require(devtools)
devtools::install_github("Ronggui/RQDA")
```

## Installation for macOS from source package

1. Open a Terminal and run the following command to install xcode: 
   ```
   sudo xcode-select —install
   ```

2. Go to http://xquartz.macosforge.org/landing/, download and install XQuartz-2.7.7.dmg.

3. Go to https://www.macports.org/install.php, download and install macport (Install MacPorts for your version of OS X, e.g Sierra)

4. Open a Terminal and run the following commands:
   ```
   sudo port install pkgconfig
   sudo port install gtk2
   ```

5. Download and install the binary version of R.

6. If the above step is successful, launch R and install RQDA from within R:
```R
install.packages('RQDA', type='source')
```

7. If all steps are successful, then we can launch RQDA by the following R command:
```R
library(RQDA) 
```

## Installation for macOS from binary package (if available on CRAN)

1. Go to http://xquartz.macosforge.org/landing/, download and install XQuartz-2.7.7.dmg.

2. Go to http://r.research.att.com, download and install the binary package of GTK+ 2.24.17. 

3. Download and install the binary version of R.

4. Launch R and install RQDA from within R:
```R
install.packages('RQDA', type='binary')
```

5. If all steps are successful, then we can launch RQDA by the following R command:
```R
library(RQDA) 
```
