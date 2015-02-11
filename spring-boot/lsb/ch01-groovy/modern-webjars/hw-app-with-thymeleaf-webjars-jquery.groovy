// Run         : spring run hw-app-with-thymeleaf-webjars-jquery.groovy

// Paramterized - http://localhost:8080/?name=Tim

@Grab("org.webjars:jquery:2.1.1")
@Grab("thymeleaf-spring4")
@Controller
class ModernWebjarsThymeLeafViewBasedApp {

  def chapters = [
    "Quick Start With Groovy",
    "Quick Start With Java",
    "Debugging and Managing Your App",
    "Data Access with Spring Boot",
    "Securing Your App"
    ]
  
  @RequestMapping("/")
  def home(
    @RequestParam(value="name", defaultValue="World") String n
    ) {
      new ModelAndView("home-modern")
      .addObject("name", n)
      .addObject("chapters", chapters)
  }
}