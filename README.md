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

1. Install Xocde from App store, launch Xcode and follow the instruction to install all the components. In addition, open a Terminal and run the following command to install xcode: 
   ```
   $ sudo xcode-select —-install
   ```

2. Go to http://xquartz.macosforge.org/landing/, download and install XQuartz-2.7.7.dmg.

3. Go to https://www.macports.org/install.php, download and install macport (Install MacPorts for your version of OS X, e.g Sierra). If you had a working MacPorts and updated the OS, they you need to migrate a MacPorts installation by following these [instructions](https://trac.macports.org/wiki/Migration). 

4. Open a Terminal and run the following commands:
   ```
   $ sudo port install pkgconfig
   $ sudo port install gtk2
   ```

5. Download and install the binary version of R.

6. If the above step is successful, launch terminal to invoke R and install RQDA from within R:

```terminal
$ R
```
```R
> install.packages('RQDA', type='source')
```

7. If all steps are successful, then we can launch RQDA by the following R command:
```R
> library(RQDA) 
```



## Installation for macOS from binary package (if available on CRAN)

1. Go to http://xquartz.macosforge.org/landing/, download and install XQuartz-2.7.7.dmg.

2. Go to http://r.research.att.com, download and install the binary package of GTK+ 2.24.17. 

3. Download and install the binary version of R.

4. Launch R and install RQDA from within R:
```R
> install.packages('RQDA', type='binary')
```

5. If all steps are successful, then we can launch RQDA by the following R command:
```R
> library(RQDA) 
```



## Dockerfile for Debian 8 

[DockerRQDA](https://github.com/FrdVnW/dockerqda) is a project providing a docker image for easily using RQDA under Debian 8.




## Portable Version for Windows Users
Download from [Dropbox Link](https://www.dropbox.com/s/5zebadz41dep09k/RQDA_0.3_1.rar?dl=0).



## How to change the font sizes of RQDA interfacea
This can be achived by modifying ~/gtkrc-2.0 (create on if not exists), for instance:
```
style "user-font" {
    font_name = "Lucida Grande 14"
}
widget_class "*" style "user-font"

gtk-font-name="Lucida Grande 14"
gtk-enable-mnemonics = 0
```
