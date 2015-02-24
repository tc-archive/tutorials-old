package tuts.secure;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

// Taken from Spring-Boot Angular Security Tutorial Series
// http://spring.io/blog/2015/01/12/spring-and-angular-js-a-secure-single-page-application#how-does-it-work

@SpringBootApplication
@RestController
public class SecureAngularApplication {

    public static void main(String[] args) {
        SpringApplication.run(SecureAngularApplication.class, args);
    }

    @RequestMapping("/resource")
    public Map<String,Object> home() {
        Map<String,Object> model = new HashMap<String,Object>();
        model.put("id", UUID.randomUUID().toString());
        model.put("content", "Hello World");
        return model;
    }
}
