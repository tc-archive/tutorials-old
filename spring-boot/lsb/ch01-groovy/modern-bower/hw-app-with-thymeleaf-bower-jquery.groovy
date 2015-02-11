// NB: Init Bower     : bower init
// NB: Install jQuery : bower install jquery --save

// Run         : spring run hw-app-with-thymeleaf-bower-jquery.groovy

// Paramterized - http://localhost:8080/?name=Tim

@Grab("thymeleaf-spring4")
@Controller
class ModernBowerThymeLeafViewBasedApp {

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