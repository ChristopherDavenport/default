package io.chrisdavenport

package object default {
  def default[A: Default]: A = Default[A].default
}