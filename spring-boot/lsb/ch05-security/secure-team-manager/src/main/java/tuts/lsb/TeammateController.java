package tuts.lsb;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.hateoas.Link;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.ModelAndView;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.StreamSupport;

import static org.springframework.hateoas.mvc.ControllerLinkBuilder.linkTo;
import static org.springframework.hateoas.mvc.ControllerLinkBuilder.methodOn;

/**
 * Created by Temple on 16/02/15.
 */
@Controller
public class TeammateController {

    private final TeammateRepository teammateRepository;

    @Autowired
    public TeammateController(TeammateRepository teammateRepository) {
        this.teammateRepository = teammateRepository;
    }

    @RequestMapping(value = "/teammates", method = RequestMethod.GET)
    public ModelAndView getTeammates() {

        ModelAndView mav = new ModelAndView("teammates");

        // Specify the view name
        mav = mav.addObject("teammates",
                // Look up ALL teammates and wrap each with related links
                StreamSupport.stream(teammateRepository.findAll().spliterator(), false)
                .map(TeammateAndLink::new)
                .toArray());

        // new Teammate command object
        mav = mav.addObject("teammate", new Teammate());


        Link createTeammateLink =
              linkTo(methodOn(TeammateController.class).newTeammate(null)).withRel("Create");
        mav = mav.addObject("postLink", createTeammateLink);


        List<Link> teammateLinks = Arrays.asList(
              linkTo(methodOn(TeammateController.class).getTeammates()).withRel("All Teammates"));
        mav = mav.addObject("links", teammateLinks);

        return mav;
    }

    @RequestMapping(value = "/teammates", method = RequestMethod.POST)
    @Secured("ROLE_ADMIN")
    public ModelAndView newTeammate(
            @ModelAttribute Teammate teammate
        ) {
        // Save the newly created teammate
        teammateRepository.save(teammate);
        // Return the All Teammates page
        return getTeammates();
    }


    @Secured("ROLE_USER")
    @RequestMapping(value = "/teammate/{id}", method = RequestMethod.GET)
    public ModelAndView getTeammate(
          @PathVariable Long id
        ) {

        // Look up the related teammate
        final Teammate teammate = teammateRepository.findOne(id);

        ModelAndView mav = new ModelAndView("teammate");

        mav.addObject("teammate", teammate);

        List<Link> teammateLinks = new ArrayList<>();

        teammateLinks.add(
              linkTo(methodOn(TeammateController.class).getTeammates())
                    .withRel("All Teammates"));

        Authentication auth = SecurityContextHolder.getContext().getAuthentication();

        if (auth.getAuthorities().stream().anyMatch(p -> p.getAuthority().equals("ROLE_ADMIN"))) {
            teammateLinks.add(linkTo(methodOn(TeammateController.class).editTeammate(id))
                  .withRel("Edit"));
        }

        mav.addObject("links", teammateLinks);

        return mav;
    }


    @RequestMapping(value = "/teammate/{id}", method = RequestMethod.PUT)
    @Secured("ROLE_ADMIN")
    public ModelAndView updateTeammate(
            @PathVariable Long id,
            @ModelAttribute Teammate teammate
        ) {

        // Connect the new teammate info with the PUT {id}
        teammate.setId(id);
        teammateRepository.save(teammate);

        // Return the teammate view
        return getTeammate(teammate.getId());
    }

    @RequestMapping(value = "/teammate/{id}/edit", method = RequestMethod.GET)
    @Secured("ROLE_ADMIN")
    public ModelAndView editTeammate(
          @PathVariable Long id
        ) {

        final Teammate teammate = teammateRepository.findOne(id);

        ModelAndView mav = new ModelAndView("edit");

        mav = mav .addObject("teammate", teammate);

        Link updateTeammateLink =
                linkTo(methodOn(TeammateController.class).updateTeammate(id, teammate))
                      .withRel("Update");
        mav = mav.addObject("putLink", updateTeammateLink);

        List<Link> teammateLinks =
                Arrays.asList(linkTo(methodOn(TeammateController.class).getTeammate(id))
                      .withRel("Cancel"));
        mav = mav.addObject("links", teammateLinks);

        return mav;
    }

}