# [Pro Spring Boot][homepage] by Felipe Gutierrez, Apress (2016)

[source code][source_code]

[homepage]: http://www.apress.com/us/book/9781484214329
[source_code]: https://github.com/felipeg48/pro-spring-boot

## 1. Introduction to Spring Boot

1.3.2, [Spring Boot][spring_boot], [reference guide][spring_boot_reference]<br>
`spring run java_or_groovy_files`, `mvn spring-boot:run`, `gradle bootRun`

[spring_boot]: http://projects.spring.io/spring-boot/
[spring_boot_reference]: http://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/

## 2. Your First Spring Boot Application

install by [SDKMAN!][sdkman] or [Homebrew][homebrew] (`brew tap pivotal/tap`)<br>
Linux Homebrew installs all the software in your home directory.<br>
`spring init -dweb,data-jpa,h2,thymeleaf --build gradle|maven myapp --force`<br>
[Spring Boot Maven Plugin][spring_boot_maven_plugin] `mvn package` (create a jar file)<br>
Maven wrapper `mvnw` and [Gradle wrapper][gradle_wrapper] `gradlew` (you do not need to install them. It will download the Maven tool in the `.mvn` subdrectory and run it.)<br>
[Spring Initializr][spring_initializr], [Spring Tool Suite][spring_tool_suite] (STS)<br>
`curl -s https://start.spring.io/starter.zip|pom.xml|build.gradle -o myapp.zip -d type=gradle-project -d dependencies=web -d packaging=jar|war`, (help) `curl start.spring.io`<br>
[Bootstrap vertical timeline style][bootstrap_vertical_timeline_style]<br>
`@SpringBootApplication` implies `@Configuration`, `@EnableAutoConfiguration` and `@ComponentScan`.

[sdkman]: http://sdkman.io/
[homebrew]: http://brew.sh/
[spring_boot_maven_plugin]: http://docs.spring.io/spring-boot/docs/current/maven-plugin/
[graddle_wrapper]: https://docs.gradle.org/current/userguide/gradle_wrapper.html
[spring_initializr]: http://start.spring.io/
[spring_tool_suite]: https://spring.io/tools/sts/
[bootstrap_vertical_timeline_style]: https://www.templatemonster.com/blog/tutorial-build-vertical-timeline-archives-page-using-bootstrap/

## 3. Spring Boot Auto-Configuration, Features, and More

(Groovy) `@EnableAutoConfiguration(exclude=[ActiveMQAutoConfiguration.class])`, (Java) `@SpringBootApplication(exclude={ActiveMQAutoConfiguration.class,DataSourceAutoConfiguration.class})`<br>
`spring init (groupId) -g=com.apres.spring (artifactId) -a=spring-boot-simple --package=com.apress.spring -name=spring-boot-simple (in the current directory) -x`

```java
// instead of SpringApplication.run(SpringBootSimpleApplication.class, args);
SpringApplication app = new SpringApplication(SpringBootSimpleApplication.class);
// ...
app.setBanner(new Banner() { // override the default "banner.txt" file
    @Override                // see also -Dbanner.location and spring.main.banner-mode=off
    public void printBanner(Environment environment, Class<?> sourceClass, PrintStream out) {
        out.print("\n\n\tThis is my own banner!\n\n".toUpperCase());
    }
});
app.setBannerMode(Mode.OFF);
new SpringApplicationBuilder()
    .bannerMode(Banner.Mode.OFF)
    .source(SpringBootSimpleApplication.class)
    .run(args);
new SpringApplicationBuilder(SpringBootSimpleApplication.class)
    .child(MyConfig.class)
    .logStartupInfo(false)
    .profiles("prod","cloud")
    .listeners(new ApplicationListener<ApplicationEvent>() {
        @Override
        public void onApplicationEvent(ApplicationEvent event) {
            log.info("#### > " + event.getClass().getCanonicalName());
        }
    })
    .web(false)
    .run(args);
// ...
app.run(args);
```

[patorjk.com][patorjk_com] (Text to ASCII Art Generator, ...)<br>
`ApplicationStartedEvent`, `ApplicationEnvironmentPreparedEvent`, `ApplicationPreparedEvent`, `ApplicationReadyEvent`, `ApplicationFailedEvent`<br>
`ApplicationArguments`.`containsOption()`, .`getNonOptionArgs()`<br>
`CommandLineRunner`'s `public void(String... args)` (or `@Bean CommandLineRunner myMethod(){ return args -> { ... }; }`), `ApplicationRunner`'s `public void run(ApplicationArguments args)`<br>
`./mvnw spring-boot:run -Drun.arguments="arg1,arg2"`<br>
[common application properties][common_application_properties]<br>
configuration property (`@Value("${data.server}")` or `org.springframework.core.env.Environment` from `org.springframework.core.env.PropertyResolver`) precedence:
1. command-line arguments, like `java ... --data.server=remoteserver:3030` or `./mvnw ... -Ddata.server=remoteserver:3030`
1. `SPRING_APPLICATION_JSON` env or `spring.application.json` argument, like `'{ "data":{"server":"remoteserver:3030"}}'`
1. JNDI (java:comp/env)
1. `System.getProperties()`
1. OS environment variables
1. `RandomValuePropertySource` (`random.*`)
1. Profile-specific (`application-{profile}.jar`) outside of the package JAR
1. Profile-specific (`application-{profile}.jar`) inside of the package JAR
1. Application properties (`application.properties` or YAML) outside of the package JAR, `./config` -> `./` -> classpath `/config` -> classpath `/`
1. Application properties (`application.properties` or YAML) inside of the package JAR
1. `@PropertySource`
1. `SpringApplication.setDefaultProperties`

`message.destinationName` == `message.destination-name` == `MESSAGE_DESTINATION_NAME`<br>
`spring.config.location`/`spring.config.name` instead of `config/application`.properties<br>
`spring.profiles.active` (or `@ActiveProfiles` and `setActiveProfiles`)<br>
`@ConfigurationProperties(prefix="myapp")` for myapp.* in application.properties

```xml
<!-- in pom.xml for code insight and code completion -->
<dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-configuration-processor</artifactId>
        <optional>true</optional>
</dependency>
```

[patorjk_com]: http://patorjk.com/
[common_application_properties]: https://docs.spring.io/spring-boot/docs/current/reference/html/common-application-properties.html

## 4. Spring Boot CLI

* `spring run` - `--watch`
* `spring test` - JUnit, [Spock][spock], org.springframework.boot.test.OutputCapture
* `spring grab` - download all the Spring Groovy scripts and Java dependencies to the `./repository` directory
* `spring jar` - jar`.original` and fat jar, Spring Boot embeds the Tomcat application server in a Spring Boot web application
* `spring war` - transportable war ( 1) standalone, 2) J2EE-compliant container deployment)
* `spring install` - download the given dependencies in a `lib` directory
* `spring uninstall`
* `spring init` - many options, (help) `--list`, or `curl start.spring.io`
* `spring shell` - start a nested shell
* `spring help`

[spock]: http://spockframework.org/

## 5. Spring with Spring Boot

(old way) `mvn archetype:create -DgroupId=com.apress.j2ee -DartifactId=simple-web-app -DarchetypeArtifactId=maven-archetype-webapp`, `mvn clean package`<br>
[Pivotal tc Server][pivotal_tc_server], [Tomcat][tomcat]<br>
In Servlet 3, `@WebServlet` instead of a `web.xml` file<br>
`@ImportResource(xml_files)`

```java
public MyApplication implements CommandLineRunner {
    public void run(String... args) {
        ...

@Bean
public CommandLineRunner runner(){ // after SpringApplication.run()
    return new CommandLineRunner() {
        public void run(String... args){
            ...

@Bean
public CommandLineRunner runner(Repository repo){
    return args -> {
        ...

@Bean
InitializingBean saveData(Repository repo){ // before CommandLineRunner
    return () -> {
        ...
```

`@EnableJms`, `@EnableCaching`, `@EnableRabbit`, `@EnableBatchProcessing`, `@EnableWebSecurity`, `@EnableRedisHttpSession`, `@EnableJpaRepositories`, `@EnableIntegration`

[pivotal_tc_server]: https://network.pivotal.io/products/pivotal-tcserver
[tomcat]: http://tomcat.apache.org/

## 6. Testing with Spring Boot

By default, the `spring-boot-starter-test` pom includes the Spring integration test, JUnit, Objenesis, Hamcrest (matcher objects), and Mockito.<br>
[Richardson Maturity Model][richardson_maturity_model]<br>
[JsonPath][jsonpath] and [Hamcrest][hamcrest]<br>
[about `MockMvc`][about_mockmvc]<br>
[REST Assured][rest_assured] (Java DSL for easy testing of REST services)

[richardson_maturity_model]: http://martinfowler.com/articles/richardsonMaturityModel.html
[jsonpath]: https://github.com/jayway/JsonPath
[hamcrest]: http://hamcrest.org/
[about_mockmvc]: http://docs.spring.io/spring-framework/docs/current/spring-framework-reference/html/integration-testing.html#spring-mvc-test-framework
[rest_assured]: https://github.com/rest-assured/rest-assured

## 7. Data Access with Spring Boot

```java
List<Journal> entries = new ArrayList();
jdbcTemplate.query("SELECT * FROM JOURNAL",
    new Object[]{},
    (rs,row) -> new Journal(rs.getLong("id"),
                            rs.getString("title"),
                            rs.getString("summary"),
                            new Date(rs.getTimestamp("created").getTime()))
).forEach(entry -> entries.add(entry));
```

by default, `org.h2.Driver`, `jdbc:h2:mem:testdb`, username: `sa`, password: empty<br>
`spring.h2.console.enabled=true` with `spring-boot-starter-web` dependency<br>
[about JPA][about_jpa] (Java Persistence API), `@Entity`, `@Id` and `@GeneratedValue(strategy=GenerationType.AUTO)`, `@Transient`<br>
Hibernate and Eclipse TopLink are the primary implementations of the JPA.<br>
There is no database or table creation; everything will be done by the abstraction of the `JournalRepository`. You don't need to implement any of these methods.<br>
[query method naming][query_method_naming], `schema.sql` and `data.sql` files<br>
(for debugging) `spring.jpa.show-sql=true`

```java
public interface JournalRepository extends JpaRepository<Journal, Long> { }

//  public List<Journal> findByTitleContaining(String word);
//  public List<Journal> findByCreatedAfter(Date date);
//  @Query("select j from Journal j where j.title like %?1%") // JPQL syntax
//  List<Journal> findByCustomQuery(String word);

public interface JpaRepository<T, ID extends Serializable> extends PagingAndSortingRepository<T, ID> {
    List<T> findAll();
    List<T> findAll(Sort sort);
    List<T> findAll(Iterable<ID> ids);
    <S extends T> List<S> save(Iterable<S> entities);
    void flush();
    <S extends T> S saveAndFlush(S entity);
    void deleteInBatch(Iterable<T> entities);
    void deleteAllInBatch();
    T getOne(ID id);
}
```

```java
// spring init -d=data-mongodb ...
import org.springframework.data.annotation.Id; // instead of javax.persistence.*
import org.springframework.data.annotation.Transient;

public interface JournalRepository extends MongoRepository<Journal, String> {
    public List<Journal> findByTitleLike(String word);

// spring.data.mongodb.database=myjournal
```

[about_jpa]: http://www.oracle.com/technetwork/articles/java/jpa-137156.html
[query_method_naming]: http://docs.spring.io/spring-data/jpa/docs/current/reference/html/#jpa.query-methods.query-creation

## 8. Web Development with Spring Boot

Spring MVC's `org.springframework.web.servlet.DispatcherServlet` is very flexible and has a very robust functionality.<br>
`spring init -d=web,thymeleaf,data-jpa,data-rest ...`

```
#Spring DataSource
spring.datasource.url = jdbc:mysql://localhost:3306/journal
spring.datasource.username = springboot
spring.datasource.password = springboot
spring.datasource.testWhileIdle = true
spring.datasource.validationQuery = SELECT 1
#JPA-Hibernate
spring.jpa.show-sql = true
spring.jpa.hibernate.ddl-auto = create-drop | create | update
spring.jpa.hibernate.naming-strategy = org.hibernate.cfg.ImprovedNamingStrategy
spring.jpa.properties.hibernate.dialect = org.hibernate.dialect.MySQL5Dialect
```

data-rest outputs an `application/hal+json` type. (Chrome JSONView add-on)<br>
profile link will redirect to the [ALPS][alps] (Application-Level Profile Semantics) metadata.

```java
@Transactional
@RepositoryRestResource(collectionResourceRel = "entry", path = "journal")
public interface JournalRepository extends JpaRepository<JournalEntry, Long> {
    List<JournalEntry> findByCreatedAfter(@Param("after") @DateTimeFormat(iso = ISO.DATE) Date date);
    List<JournalEntry> findByCreatedBetween(@Param("after") @DateTimeFormat(iso = ISO.DATE) Date after,@Param("before") @DateTimeFormat(iso = ISO.DATE) Date before);
    List<JournalEntry> findByTitleContaining(@Param("word") String word);
    List<JournalEntry> findBySummaryContaining(@Param("word") String word);
}
```

[Thymeleaf engine][thymeleaf_engine]<br>
`spring.data.rest.basePath=/api`<br>
HAL browser `spring-data-rest-hal-browser` in `http://localhost:8080/api/browser`

[alps]: http://alps.io/
[thymeleaf_engine]: http://www.thymeleaf.org/

## 9. Security with Spring Boot

`AuthenticationProvider` and `UserDetailsService`, LDAP, Active Directory, Kerberos, PAM, OAuth, ...<br>
`security.user.name = user`, `security.user.password = GUID`<br>
[security database schema][security_database_schema]

```java
@Configuration
@EnableGlobalAuthentication
public class InMemorySecurityConfiguration {
    @Autowired
    public void configureGlobal(AuthenticationManagerBuilder auth) throws Exception {
        auth.inMemoryAuthentication().withUser("user").password("password").roles("USER")
            .and().withUser("admin").password("password").roles("USER", "ADMIN");
    }
}

@Configuration
@EnableGlobalAuthentication
public class JdbcSecurityConfiguration extends GlobalAuthenticationConfigurerAdapter{
    @Bean
    public UserDetailsService userDetailsService(JdbcTemplate jdbcTemplate) {
        RowMapper<User> userRowMapper = (ResultSet rs, int i) ->
            new User(
                rs.getString("ACCOUNT_NAME"), // username
                rs.getString("PASSWORD"),     // password
                rs.getBoolean("ENABLED"),     // enabled
                rs.getBoolean("ENABLED"),     // accountNonExpired
                rs.getBoolean("ENABLED"),     // credentialsNonExpired
                rs.getBoolean("ENABLED"),     // accountNonLocked, v authorities
                AuthorityUtils.createAuthorityList("ROLE_USER", "ROLE_ADMIN"));
        return username ->
            jdbcTemplate.queryForObject("SELECT * from ACCOUNT where ACCOUNT_NAME = ?",
                userRowMapper, username);
    }

    @Autowired
    private UserDetailsService userDetailsService;

    @Override
    public void init(AuthenticationManagerBuilder auth) throws Exception {
        auth.userDetailsService(this.userDetailsService);
    }
}

@Configuration
@EnableGlobalAuthentication
public class ResourceSecurityConfiguration extends WebSecurityConfigurerAdapter{
    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http.authorizeRequests()
            .antMatchers("/").permitAll()
            .antMatchers("/api/**").authenticated()
            .and()
            .httpBasic(); // .formLogin() will redirect to http://.../login
            // .formLogin().loginPage("/login").permitAll()
            // .and()
            // .logout().permitAll();
    }
}

@RestController
public class JournalController {
    @RequestMapping(value="/login")
    public ModelAndView login(ModelAndView modelAndView){
        modelAndView.setViewName("login");
        return modelAndView;
    }
}
// or
@Configuration
static protected class LoginController extends WebMvcConfigurerAdapter{
    @Override
    public void addViewControllers(ViewControllerRegistry registry) {
        registry.addViewController("/login").setViewName("login");
    }
}
```

```html
<!-- pom.xml
    <depdendency>
        <groupId>org.springframework.security</groupId>
        <artifactId>spring-security-taglibs</artifactId>
    </depdendency>

    <depdendency>
        <groupId>org.thymeleaf.extras</groupId>
        <artifactId>thymeleaf-extras-springsecurity4</artifactId>
    </depdendency>
-->

<p sec:authorize="isAuthenticated()">
    <form th:action="@{/logout}" method="post">
        <input type="submit" value"Sign Out"/>
    </form>
</p>
```

OAuth2: Application Client -[Authorization Request]-> User Resource Owner (user) -[Authorization Grant]-> Authorization Server -[Access Token]-> Resource Server, [tutorial][spring_boot_oauth2_tutorial]

```java
/*
    <dependency>
        <groupId>org.springframework.security.oauth</groupId>
        <artifactId>spring-security-oauth2</artifactId>
    </dependency>
*/
@Configuration
@EnableAuthorizationServer // /oauth/authorize, /oauth/token
@EnableResourceServer
public class ResourceOAuthSecurityConfiguration extends ResourceServerConfigurerAdapter{
    @Override
    public void configure(HttpSecurity http) throws Exception {
        http.authorizeRequests()
            .antMatchers("/").permitAll()
            .antMatchers("/api/**").authenticated();
    }
}
```

```
$ curl -i localhost:8080/oauth/token -d "grant_type=password&scope=read&username=springboot&password=isawesome" -u <security.oauth2.client.clientId GUID>:<security.oauth2.client.secret GUID>
{"access_token":"f1d3...","token_type":"bearer","refresh_token":"2d34...","expires_in":43199,"scope":"write"}
$ curl -i -H "Authorization: bearer f1d3..." localhost:8080/api
```

[security_database_schema]: http://docs.spring.io/spring-security/site/docs/current/reference/html/appendix-schema.html
[spring_boot_oauth2_tutorial]: https://spring.io/guides/tutorials/spring-boot-oauth2/

## 10. Messaging with Spring Boot

```xml
<dependency>
    <groupId>org.springframework.boot</groupId> <!-- -d=hornetq -->
    <artifactId>spring-boot-starter-hornetq</artifactId>
</dependency>

<dependency>
    <groupId>org.hornetq</groupId> <!-- embedded hornetq broker -->
    <artifactId>hornetq-jms-server</artifactId>
</dependency>
```

```
spring.hornetq.mode=embedded
spring.hornetq.embedded.enabled=true
spring.hornetq.embedded.queues=springbootQueue,pivotalQueue

myqueue=springbootQueue

#spring.hornetq.mode=native
#spring.hornetq.host=192.168.1.10
#spring.hornetq.port=9876
```

```java
@JmsListener(destination="${myqueue}") // instead of MessageListener.onMessage()
@SendTo("${myotherqueue}") // instead of JmsTemplate.convertAndSend()
public String simplerConsumer(String message){
    log.info("Simpler Consumer> " + message);
    return message + " and Spring Messaging too!";
}
```

RabbitMQ/AMQP - exchange types (direct (one-to-one), topic (wildcard), headers (any expressions), fanout (broadcast)), bindings (some rules), [tutorial][amqp_concepts]<br>
`spring-boot-starter-amqp` (`-d=amqp`) includes spring-amqp and rabbitmq-client libraries.
`RabbitTemplate` and `RabbitListener` instead of `JmsTemplate` and `JmsListener`<br>
`org.springframework.amqp.support.converter.MessageConverter` interface, [RabbitMQ Java client library][rabbitmq_java_client_library]<br>
`rabbitmq-server`, `rabbitmq-plugins enable rabbitmq_management` (web console manager), `http://localhost:15672` `guest:guest`<br>
`@EnableScheduling`, `@Scheduled(fixedDelay = 500L)`<br>
[Spring AMQP][spring_amqp]

```
spring.rabbitmq.host=mydomain.com
spring.rabbitmq.username=rabbituser
spring.rabbitmq.password=thisissecured
spring.rabbitmq.port=5672
spring.rabbitmq.virtual-host=/production
```

REmote DIctionary Server publish/subscribe messaging system, [Spring Data Redis][spring_data_redis]<br>
`spring-boot-starter-redis` (`-d=redis`) includes spring-data-redis libraries.<br>
`StringRedisTemplate`, `MessageListenerAdapter` -> `RedisMessageListenerContainer`

```
spring.redis.database=0
spring.redis.host=localhost
spring.redis.password=mysecurepassword
spring.redis.port=6379
```

`spring-boot-starter-websocket` (`-d=websocket`) includes spring-webmvc, spring-messaging, spring-websocket. You need not to include spring-boot-starter-web.<br>
`SimpleMessaingTemplate` (`SimpMessagesSendingOperations`)<br>
[STOMP][stomp], [SockJS][sockjs]<br>
[jsDelivr][jsdelivr], [cdnjs.com][cdnjs_com]

```java
@Configuration
@EnableWebSocketMessageBroker
public class WebSockConfig extends AbstractWebScoketMessageBrokerConfigurer{
    @Override
    public void registerStompEndpoints(StompEndpointRegistry registry) {
        registry.addEndpoint("/stomp").withSockJS();
    }

    @Override
    public void configureMessageBroker(MessageBrokerRegistry config) {
        config.enableSimpleBroker("/topic");
        config.setApplicationDestinationPrefixes("/app");
    }
}
```

[amqp_concepts]: http://www.rabbitmq.com/tutorials/amqp-concepts.html
[rabbitmq_java_client_library]: http://www.rabbitmq.com/java-client.html
[spring_amqp]: http://projects.spring.io/spring-amqp/
[spring_data_redis]: http://projects.spring.io/spring-data-redis/
[stomp]: http://stomp.github.io/
[sockjs]: https://github.com/sockjs
[jsdelivr]: http://www.jsdelivr.com/
[cdnjs_com]: https://cdnjs.com/

## 11. Spring Boot Actuator

## 12. Deploying Spring Boot

## 13. Spring Boot in the Cloud

## 14. Extending Spring Boot Apps

## A. Spring Boot 1.4.x

