# default [![Build Status](https://travis-ci.com/ChristopherDavenport/default.svg?branch=master)](https://travis-ci.com/ChristopherDavenport/default) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/default_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/default_2.12)

A typeclass approach for default values in Scala. The topic of much scorn in Haskell, I find the existence of this typeclass
to be superior to the general use Monoid everywhere as though it was a default value.

Default exists to allow you to have something that expresses what you want, even if it might not be what you actually need.

Default exists as a typeclass that allows `Default[A].default: A`, it exposes in the package object `def default[A: Default]: A` which you will likely find useful to remove the ceremony of summoning the typeclass to use.