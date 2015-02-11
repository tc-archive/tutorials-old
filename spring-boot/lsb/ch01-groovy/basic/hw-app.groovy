// Run         : spring run hw-app.groovy
// Package     : spring jar hw-app.jar hw-app.groovy
// Run Package : java -jar hw-app.jar

@RestController
class HelloWorldApp {
  @RequestMapping("/")
  def home() {
    "Hello, world!"
  }
}