# [가장 빨리 만나는 스프링 부트][homepage], 마키 토시아키 저, 김범준 역, 길벗 (2015)

はじめてのSpring Boot, 槇 俊明, 工学社 (2014), 2016년 개정판 출판<br>
버전 1.2.4, [source code][source_code], [sample code by the author][sample_code], [Japanese slide][japanese_slide]

[homepage]: http://www.gilbut.co.kr/book/bookView.aspx?bookcode=BN001243
[source_code]: https://github.com/gilbutITbook/006815
[sample_code]: https://github.com/making/hajiboot-samples
[japanese_slide]: http://www.slideshare.net/makingx/spring-boot-java-jsug

## 1장 스프링 부트 개요

influenced by [Dropwizard][dropwizard]<br>
[spring-boot-samples][spring_boot_samples] at GitHub<br>
`mvn dependency:tree`<br>
`mvn spring-boot:run` (Run As > Spring Boot App), `mvn package`, `java -jar target\hajiboot-1.0.0-SNAPSHOT.jar --server.port=8888`<br>
[Gradle build][gradle_build]<br>
Spring Loaded

```xml
...
<build>
    <plugins>
        <plugin>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-maven-plugin</artifactId>
            <dependencies>
                <dependency>
                    <groupId>org.springframework</groupId>
                    <artifactId>springloaded</artifactId>
                    <version>1.2.3.RELEASE</version>
                </dependency>
            </dependencies>
        </plugin>
    </plugins>
</build>
...
```

[dropwizard]: http://www.dropwizard.io/
[spring_boot_samples]: https://github.com/spring-projects/spring-boot/tree/master/spring-boot-samples
[gradle_build]: http://docs.spring.io/spring-boot/docs/current/reference/html/using-boot-build-systems.html#using-boot-gradle

## 2장 스프링 프레임워크 금방 배우기

Lombok `@Data` (`@RequiredArgsConstructor`, `@AllArgsConstructor`, `@NoArgsConstructor`)

```xml
<dependency>
    <groupId>org.projectlombok</groupId>
    <artifactId>lombok</artifactId>
    <version>1.16.4</version>
    <scope>provided</scope>
</dependency>
```

XML or JavaConfig (`@Configuration` and `@Bean`, `@Import(AppConfig.class)`)<br>
get beans from a DI container (`context.getBean()`) -> DI (`@Autowired`) -> component scan (without AppConfig class, `@Component`, `@ComponentScan`) -> implement `CommandLineRunner.run(...)`<br>
component scan scans `@Component`, `@Controller` (`@RestController` in Spring 4), `@Service` and `@Repository` (that converts internal exceptions into `DataAccessException`).<br>
`schema-<platform>.sql`, `schema.sql`, `data-<platform>.sql`, `data.sql`, `--spring.datasource.platform=all`<br>
`spring.datasource.sqlScriptEncoding: UTF-8`

```java
String sql = "SELECT :a + :b";
SqlParameterSource param = new MapSqlParameterSource()
    .addValue("a", 100)
    .addValue("b", 200);
// SqlParameterSource param = new BeanPropertySqlParameterSource(customer);
Integer result = jdbcTemplate.queryForObject(sql, param, Integer.class);

Customer result = jdbcTemplate.queryForObject(sql, param,
    new RowMapper<Customer>() {
        @Override
        public Customer mapRow(ResultSet rs, int rowNum) throws SQLException {
            return new Customer(rs.getInt("id"), rs.getString("first_name"),
                rs.getString("last_name"));
        }
    }
//  (rs, rowNum) -> new Customer(rs.getInt("id"),
//      rs.getString("first_name"), rs.getString("last_name"))
);
```

```yml
spring:
  datasource:
    driverClassName: org.h2.Driver
    url: jdbc:h2:mem:testdb;DB_CLOSE_DELAY=-1;DB_CLOSE_ON_EXIT=FALSE
       # jdbc:h2:file:/tmp/testdb
    username: sa
    password:
```

Log4JDBC (log4jdbc-remix -> [log4jdbc-log4j2][log4jdbc_log4j2])

```xml
<dependency>
    <groupId>org.lazyluke</groupId>
    <artifactId>log4jdbc-remix</artifactId>
    <version>0.2.7</version>
</dependency>

<!-- logback.xml -->
<configuration>
    <include resource="org/springframework/boot/logging/logback/base.xml"/>
    <logger name="jdbc" level="OFF"/> <!-- SQL logs only -->
    <logger name="jdbc.sqltiming" level="DEBUG"/>
</configuration>
```

`@Transactional` does not rollback on checked exceptions.

```java
SimpleJdbcInsert insert = new SimpleJdbcInsert(
    (JdbcTemplate) jdbcTemplate.getJdbcOperations())
    .withTableName("customers")
    .usingGenerateKeyColumns("id");
insert.executeAndReturnKey(param);
```

`@Query("jpql")`, `@Query(value = "sql", nativeQuery = true)`, [Spring Data JPA documentation][spring_data_jpa_doc]


```java
Pageable pageable = new PageRequest(0, 3);
Page<Customer> page = customerRespository.findAll(pageable);
// page.getSize(), .getNumber(), .getTotalPages(), .getTotalElements(), .getContent()

public interface CustomerRepository extends JpaRepository<Customer, Integer> {
    @Query("SELECT x FROM Customer x ORDER BY x.firstName, x.lastName")
    List<Customer> findAllOrderByName();
}
```

[log4jdbc_log4j2]: https://code.google.com/archive/p/log4jdbc-log4j2/
[spring_data_jpa_doc]: http://docs.spring.io/spring-data/jpa/docs/current/reference/html/

## 3장 스프링 부트로 웹 애플리케이션 개발하기

[Spring MVC][spring_mvc], [Spring Data Commons][spring_data_commons]

```java
// class CustomerRestController
@RequestMapping(method = RequestMethod.POST)
@ResponseStatus(HttpStatus.CREATED)
Customer postCustomers(@RequestBody Customer customer) {
    return customerService.create(customer);
}

@RequestMapping(method = RequestMethod.POST)
ResponseEntity<Customer> postCustomers(@RequestBody Customer customer,
        UriComponentsBuilder uriBuilder) {
    Customer created = customerService.create(customer);
    URI location = uriBuilder.path("api/customers/{id}")
        .buildAndExpand(created.getId()).toUri();
    HttpHeaders headers = new HttpHeaders();
    headers.setLocation(location);
    return new RepsonseEntity<>(created, headers, HttpStatus.CREATED);
}

@RequestMapping(method = RequestMethod.GET)
Page<Customer> getCustomers(@PageableDefault Pageable pageable) {
    Page<Customer> customers = customerService.findAll(pageable);
    return customer;
}
```

On Spring Boot development, I prefer Thymeleaf (`spring-boot-starter-thymeleaf`) to JSP with constraints.<br>
`spring.thymeleaf.cache: false`<br>
Bean Validation (javax.validation.constraints.* annotations)<br>
Thymeleaf Layout Dialect (use fragments)

```java
// class CustoemrController
@ModelAttribute
CustomerForm setUpForm() {
    return new CustomerForm();
}

@RequestMapping(value = "create", method = RequestMethod.POST)
String create(@Validated CustomerForm form, BindingResult result, Model model) {
    if (result.hasErrors()) {
        return list(model);
    }
    Customer customer = new Customer();
    BeanUtils.copyProperties(form, customer); // may use Dozer or ModelMapper
    customerService.create(customer);
    return "redirect:/customers";
}
```

```java
// org.springframework.web.filter.CharacterEncodingFilter servlet filter configuration
@Configuration
public class AppConfig {
    @Order(Ordered.HIGHEST_PRECEDENCE)
    @Bean
    CharacterEncodingFilter characterEncodingFilter() {
        CharacterEncodingFilter filter = new CharacterEncodingFilter();
        filter.setEncoding("UTF-8");
        filter.setForceEncoding(true);
        return filter;
    }
}
```

```
<dependency>
    <groupId>org.webjars</groupId>
    <artifactId>bootstrap</artifactId>
    <version>3.3.4</version>
</dependency>

<link ref="stylesheet" type="text/css" th:href="@{/webjars/bootstrap/3.3.4/css/bootstrap.min.css}"/>
<!-- or http://cdn.jsdelivr.net/webjars/bootstrap/3.3.4/js/bootstrap.min.js -->
```

[Flyway][flyway] database migration, `flyway-core` dependency, src/main/resources/db/migration/V(version number)__(script name).sql<br>
`UPDATE "PUBLIC"."schema_Version" SET "version_rank" = "version_rank" + 1 WHERE "vesion_rank" >= 1` executed<br>
[Spring Security document][spring_security_doc]<br>
disable basic authentication, `security.basic.enabled: false`<br>
to prevent 'N+1 select problem', `@Query("SELECT DISTINCT x FROM Customer x JOIN FETCH x.user ORDER BY x.firstName, x.lastName")`<br>
[thymeleaf-extras-springsecurity][thymeleaf_extras_springsecurity]

```java
public class LoginUserDetails extends org.springframework.security.core.userdetails.User {
    // ....userdetails.UserDetails, .UserDetailsService, .UsernameNotFoundException
    private final User user;

    public LoginUserDetails(User user) {
        super(user.getUsername(), user.getEncodedPassword()),
            AuthorityUtils.createAuthorityList("ROLE_USER"));
        this.user = user;
    }
}

@Configuration
@EnaleWebMvcSecurity
public class SecurityConfig extends WebSecurityConfigurerAdapter {
    @Override
    public void configure(WebSecurity web) throws Exception {
        web.ignoring().antMatchers("/webjars/**", "/css/**");
    }

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http.authorizeRequests()
            .antMatchers("/loginForm").permitAll()
            .anyRequest().authenticated();
        http.formLogin()
            .loginProcessingUrl("/login")
            .loginPage("/loginForm")
            .failureUrl("/loginForm?error")
            .defaultSuccessUrl("/customers", true)
            .usernameParameter("username").passwordParameter("password")
            .and();
        http.logout()
            .logoutRequestMatcher(new AntPathRequestMatcher("/logout**"))
            .logoutSuccessUrl("/loginForm");
    }

    @Configuration
    static class AuthenticationConfiguration extends GlobalAuthenticationConfigurerAdapter {
        @Autowired
        UserDetailsService userDetailsService;

        @Bean
        PasswordEncoder passwordEncoder() {
            return new BCryptPasswordEncoder();
        }

        @Override
        public void init(AuthenticationManagerBuilder auth) throws Exception {
            auth.userDetailsService(userDetailsService)
                .passwordEncoder(passwordEncoder());
        }
    }
}
```

```java
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

public class GenPassword{
    public static void main(String[] args) {
        System.out.println(new BCryptPasswordEncoder().encode("demo"));
    }
}

/* if there are many main functions, in pom.xml
    <plugin>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-maven-plugin</artifactId>
        <configuration>
            <mainClass>com.example.App</mainClass>
        </configuration>
    </plugin>
*/
```

[spring_mvc]: http://docs.spring.io/spring-framework/docs/current/spring-framework-reference/html/mvc.html
[spring_data_commons]: http://docs.spring.io/spring-data/data-commons/docs/current/reference/html/
[flyway]: https://flywaydb.org/
[spring_security_doc]: http://docs.spring.io/spring-security/site/docs/current/reference/htmlsingle/
[thymeleaf_extras_springsecurity]: https://github.com/thymeleaf/thymeleaf-extras-springsecurity

## 4장 PaaS 헤로쿠에 디플로이하기

[Heroku CLI][heroku_cli] (Heroku client, Forman, Git), PostgreSQL<br>
in Procfile, `web: java $JAVA_OPTS -jar target/*.jar --server.port=$PORT`<br>
in system.properties, `java.runtime.version=1.8`<br>
`heroku login`, `heroku create`, `git push heroku master`, `heroku open`, `heroku logs -t`, `heroku info`<br>
[CORS][cors] ([guide][cors_guide])<br>
[Spring Cloud Connectors][spring_cloud_connectors]<br>
Spring Boot Actuator, `spring-boot-starter-actuator`, /metrics /health /dump /configprops /env

[heroku_cli]: https://devcenter.heroku.com/articles/heroku-cli
[cors]: http://spring.io/understanding/CORS
[cors_guide]: http://spring.io/guides/gs/rest-service-cors/
[spring_cloud_connectors]: http://cloud.spring.io/spring-cloud-connectors/

## 5장 스프링 부트로 테스트하기

[Spring MVC Test Framework][spring_mvc_test_framework]

```java
@RunWith(SpringJUnit4ClassRunner.class)
@SpringApplicationConfiguration(classes = App.class)
@WebAppConfiguration // run an embedded web server
@IntegrationTest({"server.port:0", // override configuration
    "spring.datasource.url:jdbc:h2:mem:bookmark;DB_CLOSE_ON_EXIT=FALSE"})
public class AppTest {
    @Value("${local.server.port}")
    int port;
    RestTemplate restTemplate = new TesstRestTemplate(); // RestTemplate + a

    @Test
    public void testHome() {
        ResponseEntity<String> response = restTemplate.getForEntity(
            "http://localhost:" + port, String.class);
        assertThat(response.getStatusCode(), is(HttpStatus.OK));
        assertThat(response.getBody(), is("Hello World!"));

        ResponseEntity<Page<Customer>> response = restTemplate.exchange(
            apiEndpoint, HttpMethod.GET, null /* body,header */,
            new ParameterizedTypeReference<Page<Customer>>() {
            });

        ResponseEntity<Customer> response = restTemplate.exchange(apiEndpoint,
            HttpMethod.POST, new HttpEntity<>(customer3), Customer.class);

        ResponseEntity<Void> response = restTemplate.exchange(apiEndpoint + "/{id}",
            HttpMethod.DELETE, null,
            Void.class, Collections.singletonMap("id", customer1.getId()));
    }
}
```

[REST-assured][rest_assured]

```java
when.get("api/customers")
    .then()
    .statusCode(HttpStatus.OK.value())
    .body("numberOfElements", is(2))
    .body("content[0].id", is(customer2.getId()));

given().body(customer3)
    .contentType(ContentType.JSON)
    .and()
    .when().post("/api/customers")
    .then()
    ....;
```

[spring_mvc_test_framework]: http://docs.spring.io/spring/docs/current/spring-framework-reference/html/integration-testing.html#spring-mvc-test-framework
[rest_assured]: https://github.com/rest-assured/rest-assured

## 부록 A 소프트웨어 설치 방법

double-click lombok.jar to launch an installer

## 부록 B 그레이들로 빌드하기

`gradle tasks` (run, bootRun, build, clean, depedencies, wrapper), `gradle -gui`<br>
[sprng boot version dependency pom file][spring_boot_version_dependency_file]<br>
[propdeps plugin][propdeps] (enable `provided` or `optional` dependencies)<br>
[gradle user guide][gradle_user_guide]

[spring_boot_version_dependency_file]: https://github.com/spring-projects/spring-boot/blob/master/spring-boot-dependencies/pom.xml
[propdeps]: https://github.com/spring-projects/gradle-plugins/tree/master/propdeps-plugin
[gradle_user_guide]: https://docs.gradle.org/current/userguide/userguide

