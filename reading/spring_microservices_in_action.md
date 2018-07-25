# [Spring Microservices in Action][homepage] by John Carnell, Manning (2017)

[source code][source_code]

[Architectural Styles and the Design of Network-based Software Architectures][rest_paper]

[homepage]: https://www.manning.com/books/spring-microservices-in-action
[source_code]: https://github.com/carnellj/spmia_overview
[rest_paper]: https://www.ics.uci.edu/~fielding/pubs/dissertation/top.htm

## 1. Welcome to the cloud, Spring

This command, `mvn spring-boot:run`, will use a Spring Boot plug-in to start the
 application using an embedded Tomcat server.<br>
The Spring Boot framework has strong support for both Java and the Groovy
 programming languages. You can build microservices with Groovy and no project
 setup.<br>
Small, Simple, and Decoupled Services = Scalable, Resilient, and Flexible
 Applications<br>
Phoenix servers---The longer a server is running, the more opportunity for
 configuration drift. How do you ensure that servers that run microservices get
 torn down on a regular basis and recreated oof an immutable image?<br>
upper environments, such as stage or production<br>
Spring Cloud wraps the work of open source companies such as Pivotal, HashiCorp,
 and Netflix in delivering patterns.<br>
Spring Cloud service discovery can be implemented using Consul and Eureka as its
 service discovery engine.<br>
White the Netflix Ribbon project simplifies integrating with service discovery
 agents such as Eureka, it also provides client-side load-balancing of service
 calls from a service consumer. This makes it possible for a client to continue
 making service calls even if the service discovery agent is temporarily
 unavailable.<br>
Spring Cloud uses the Netflix Zuul project to provide service routing
 capabilities for your microservice application. With Spring Cloud Stream, you
 can quickly integrate your microservices with message broker such as RabbitMQ
 and Kafka.<br>
Spring Cloud Sleuth allows you to integrate unique tracking identifiers into the
 HTTP calls and message channels (RabbitMQ, Apache Kafka) being used within your
 application. These trace IDs, sometimes referred to as correlation, are
 automatically added to any logging statements you make in your microservice.
 The real beauty of Spring Cloud Sleuth is seen when it's combined with logging
 aggregation technology tools such as Papertrail and tracing tools such as
 Zipkin. Papertail is a cloud-based logging platform used to aggregate logs in
 real time from different microservices into one queryable database. Open Zipkin
 takes data produced by Spring Cloud Sleuth and allows you to visualize the flow
 of your service calls involved for a single transaction.<br>
The JavaScript Web Token (JWT) framework standardizes the format of how a OAuth2
 token is created and provides standars for digitally signing a created
 token.<br>
The presence of the `@EnableEurekaClient` has told Spring Boot that you're going
 to use a modified `RestTemplate` class whenever you make a REST service call.
 Also, the `RestTemplate` class is using Netflix's Ribbon library. Ribbon will
 retrieve a list of all the physical endpoints associated with a serivce. By
 eliminating a centralized load balancer and moving it to the client, you
 eliminate another failure point in your application infrastructure.<br>
Spring Boot is used to simplify the building of REST-based/JSON microservices.

## 2. Building microservices with Spring Boot

(*Abstracted*) Microservices completely own their data structures and data
 sources. Data owned by a microservice can only be modified by that service.<br>
The watchwords for the DevOps engineer are *consistency* and *repeatability* in
 every environment.<br>
Microservices are an expression of business logic and not an abstraction layer
 over your data sources. If your microservices do nothing but CRUD-related
 logic, they're probably too fine-grained.<br>
In a Spring Boot application, you can define Spring Beans by 1) annotating a
 Java class with a `@Component`, `@Service` or `@Repository` annotation tag or
 2) annotating a class with a `@Configuration` tag and then defining a
 constructor method for each Spring Bean you want to build with a `@Bean`
 tag. Under the covers, the `@SpringBootApplication` annotation marks the
 Application class as a configuration class, then begin auto-scanning all the
 classes on the Java class path for other Spring Beans. You aren't doing
 anything with the `ApplicationContext`, so it isn't shown in the code.<br>
REST-based services take advantage of these HTTP status codes and other
 web-based infrastructure, such as reverse proxies and caches, which can be
 integrated with your microservices with relative ease.<br>
`@RestController` tells Spring Boot this is a REST-based services and will
 automatically serialize/deserialize service request/response to JSON. Unlike
 the traditional Spring `@Controller` annotation, the `@RestController`
 annotation doesn't require you as the developer to return a `ResponseBody`
 class from your controller class.<br>
From a DevOps perspective, operational lifecycle steps - service assembly,
 service bootstrapping, service registration/discovery, service monitoring.<br>
In Spring Boot, exposing an endpoint that will return the health of the service
 is trivial and involves nothing more than modifying your Maven build file to
 include the Spring Actuator module.

## 3. Controlling your configuration with Spring Cloud configuration server

four principles - segregate, abstract, centralize, harden (highly available and
 redundant)<br>
Configuration information should either be passed to the starting service as
 environment variables or read from a centralized repository when the service
 starts.<br>
Eureka - offers dynamic client refresh out of the box<br>
Consul - [SWIM protocol][swim_protocol]<br>
Spring Cloud configuration server - can use multiple back ends for storing
 configuration data including a shared filesystem, Eureka, Consul, and Git<br>
The Spring Cloud configuration server is a REST-based application that's built
 on top of Spring Boot.

```xml
<dependencyManagement>
  <dependencies>
    <dependency>
      <groupId>org.springframewoek.cloud</groupId>
      <artifactId>spring-cloud-dependencies</artifactId>
      <version>Camden.SR5</version>
      <type>pom</type>
      <scope>import</scope>
    </dependency>
  </dependencies>
</dependencyManagement>

<dependencies>
  <dependency>
    <groupId>org.springframework.cloud</groupId>
    <artifactId>spring-cloud-starter-config</artifactId>
  </dependency>

  <dependency>
    <groupId>org.springframework.cloud</groupId>
    <artifactId>spring-cloud-config-server</artifactId>
  </dependency>
</dependencies>
```

Spring Cloud uses a non-traditional mechanism for labelling Maven projects.
 Spring Cloud is a collection of independent subprojects. The Spring Cloud team
 does their releases through what they call the "release train." All the
 subprojects that make up Spring Cloud are packaged under one Maven biil of
 materials (BOM) and released as a whole. The Spring Cloud team has been using
 the name of London subway stops as the name of their releases, with each
 incrementing major release giving a London subway stop that has the next
 highest letter. There have been three releases: Angel, Brixton, and Camden.

`@EnableConfigServer`

```yaml
# confsvr/src/main/resources/application.yml
# confsvr/src/main/resources/config/licensingservice/licensingservice[-dev].yml
# http://localhost:8888/licensingservice/{default,dev}
server:
   port: 8888
spring:
  profiles:
    active: native
  cloud:
     config:
       server:
           native:
             searchLocations: file:///Users/...
           git:
             url: https://github.com/carnellj/config-repo/
             searchPaths: licensingservice,organizationservice
             username: native-cloud-apps
             password: 0ffended
```

When the licensing service is first started, you'll pass it via the command line
 two pieces of information: the Spring profile and the endpoint the licensing
 serivce should use to communicate with the Spring Cloud configuration
 service.<br>
The `bootstrap.yml` file reads the application properties before any other
 configuration information used. In general, the `bootstrap.yml` file contains
 the application name for the service (`spring.application.name`), the
 application profile (`spring.profiles.active`), and the URI to connect to a
 Spring Cloud Config server (`spring.cloud.config.uri`). Any other configuration
 information that you want to keep local to the service (and not stored in
 Spring Cloud Config) can be set locally in the services in the
 `application.yml` file. Usually, the information you store in the
 `application.yml` file is configuration data that you might want to have
 available to a service even if the Spring Cloud Config service is
 unavailable.

```java
package com.thoughtmechanix.licenses.model;

@Entity
@Table(name = "licenses")
public class License {
  @Id
  @Column(name = "license_id", nullable = false)
  private String licenseId;

  @Column(name = "organization_id", nullable = false)
  private String organizationId;

  @Column(name = "product_name", nullable = false)
  private String productName;
}

package com.thoughtmechanix.licenses.repository;

@Repository
public interface LicenseRepository extends CrudRepository<License,String>
{
  public List<License> findByOrganizationId(String organizationId);
  public License findByOrganizationIdAndLicenseId(String organizationId, String licenseId);
}

package com.thoughtmechanix.licenses.services;

@Service
public class LicenseService {
  @Autowired
  private LicenseRepository licenseRepository;

  @Autowired
  ServiceConfig config;

  public License getLicense(String organizationId, String LicenseId) {
    License license = licenseRepository.findByOrganizationIdAndLicenseId(organizationId, licenseId);
    return license.withComment(config.getExampleProperty());
  }

  public List<License> getLicensesByOrg(String organizationId) {
    return licenseRepository.findByOrganizationId(organizationId);
  }

  public void saveLicense(License license) {
    license.withId(UUID.randomUUID().toString());
    licenseRepository.save(license);
  }
}

package com.thoughtmechanix.linceses.config;

@Component
public class ServiceConfig {
  @Value("${example.property}")
  private String exampleProperty;

  public String getExampleProperty() {
    return exampleProperty;
  }
}
```

Spring Boot Actuator does offer a `@RefreshScope` annotation that will allow a
 development team to access a `/refresh` endpoint that will force the Spring
 Boot application to reread its application configuration. Items such as your
 database configuration that are used by Spring Data won't be reloaded by the
 `@RefreshScope` annotation.<br>
Spring Cloud configuration service does offer a "push"-based mechanism called
 Spring Cloud Bus that will allow the Spring Cloud configuration server to
 publish to all the clients using the service that a change has occurred. Spring
 Cloud configuration requires an extra piece of middleware running (RabbitMQ).
 Not all Spring configuration backends support the "push" mechanism (that is,
 the Consul server).<br>
When you fire up your Spring Cloud Config instance, Spring Cloud Config detects
 that the `ENCRYPT_KEY` environment variable is set and automatically adds two
 new endpoints (`/encrypt` and `/decrypt`) to the Spring Cloud Config service
 Spring Cloud configuration server requires all encrypted properties to be
 prepended with a value of `{cipher}`.

[swim_protocol]: https://pdfs.semanticscholar.org/8712/3307869ac84fc16122043a4a313604bd948f.pdf

## 4. On service discovery

[UDDI][uddi] (Universal Description, Discovery, and Integration) repository<br>
The serivce discovery's client should "cache" service information locally.<br>
[gossip protocol][gossip_protocol]

```yaml
# for Eureka server, @EnableEurekaServer
server:
  port: 8761
eureka:
  client:
    registerWithEureka: false # because this is the Eureka service
    fetchRegistry: false # cache
  server:
    #waitTimeInMsWhenSyncEmpty: 5

# for Eureka client
eureka:
  instance:
    preferIpAddress: true # ip address rather than its hostname (default: false)
  client:
    registerWithEureka: true
    fetchRegistry: true # re-contaca every 30 seconds
    serviceUrl:
      defaultZone: http://localhost:8761/eureka/ # + apps/organizationservice
```

Eureka will wait five minutes by default to give all of the services a chance to
 register with it before advertising them. Individual services registering will
 take up to 30 seconds to show up in the Eureka serivce because Eureka requires
 three consecutive heartbeat pings from the service spaced 10 seconds apart
 before it will say the service is ready for use.<br>
Every service registered with Eureka will have two components associated with
 it: the application ID and the (random) instance ID.<br>
The libraries we'll explore include Spring Discovery client, Spring Discovery
 client enabled RestTemplate, and Netflix Feign client.<br>
The `@EnableDiscoveryClient` annotation is the trigger for Spring Cloud to
 enable the application to use the DiscoveryClient and Ribbon libraries.

```java
RestTemplate restTemplate = new RestTemplate();
ResponseEntity<Organization> restExchange = restTemplate.exchange(
          serviceUri, HttpMethod.GET, null, Organization.class, organizationId);
return restExchange.getBody();
```

`@Autowired(required = false)`<br>
Once you've enabled the Spring DiscoveryClient in the application class via the
 `@EnableDiscoveryClient` annotation, all RestTemplates managed by the Spring
 framework will have a Ribbon-enabled interceptor injected into them that will
 change how URLs are created with the RestTemplate class. Directly instantiating
 the RestTemplate class allows you to avoid this behavior.<br>
To use a Ribbon-aware `RestTemplate` class, you need to define a RestTemplate
 bean construction method with a Spring Cloud annotation called `@LoadBalanced`.

[uddi]: https://en.wikipedia.org/wiki/Web_Services_Discovery#Universal_Description_Discovery_and_Integration
[gossip_protocol]: https://en.wikipedia.org/wiki/Gossip_protocol

## 5. When bad things happen: client resiliency patterns with Spring Cloud and Netflix Hystrix

There are four client resiliency patterns: client-side load balancing, circuit
 breakers, fallbacks and bulkheads.<br>
If the client-side load balancer detects a problem, it can remove that service
 instance from the pool of available service locations and prevent any future
 service calls from hitting that service instance. This is exactly the behavior
 that Netflix's Ribbon libraries provide out of the box with no extra
 configuration.<br>
The thread pools act as the bulkheads for your service. Each remote resource is
 segregated and assigned to the thread pool. If one service is responding
 slowly, the thread pool for that one type of service call will become saturated
 and stop processing requests.<br>
When the Spring framework see the `@HystrixCommand`, it will dynamically
 generate a proxy that will wrapper the method and manage all calls to that
 method through a thread pool of threads specifically set aside to handle remote
 calls.<br>
By default, all Hystrix commands will share the same thread pool to process
 requests. This thread pool will have 10 threads in it.

```java
@HystrixCommand(
  commandProperties = {
    @HystrixProperty(name="execution.isolation.thread.timeoutInMilliseconds", value="12000") // default 1000
  },
  fallbackMethod = "buildFallbackLicenseList",
  threadPoolKey = "licenseByOrgThreadPool",
  threadPoolProperties = {
    @HystrixProperty(name="coreSize", value="10"),
    // queue size for the number of requests that can queue if the individual threads are busy
    @HystrixProperty(nme="maxQueueSize", value="-1")
  },
  commandPoolProperties = {
    @HystrixProperty(name="circuitBreaker.requestVolumeThreshold", value="20"),
    @HystrixProperty(name="circuitBreaker.errorThresholdPercentage", value="50"),
    @HystrixProperty(name="circuitBreaker.sleepWindowInMilliseconds", value="5000"),
    @HystrixProperty(name="metrics.rollingStats.timeInMilliseconds", value="10000"),
    @HystrixProperty(name="metrics.rollingStats.numBuckets", value="10")
  }
)
```

If you set the `maxQueueSize` to -1, a Java `SynchronousQueue` will be used to
 hold all incoming requests. if set to -1, no queue is used and instead Hystrix
 will block until a thread becomes available for processing. Setting the
 `maxQueueSize` to a value greater than one will cause Hystrix to use a Java
 `LinkedBlockingQueue`. Hystrix does allow you to dynamically change the size of
 the queue by using the `queueSizeRejectionThreshold` attribute, but this
 attribute can only be set when the `maxQueueSize` attribute is a value greater
 than 0.<br>
Netflix recommends the proper sizing for a custom thread pool: (requests per
 second at peak when the service is healthy * 99th percentile latency in
 seconds) + small amount of extra threads for overhead<br>
Hystrix's default isolation model, THREAD, completely isolates a Hystrix
 protected call, but doesn't propagate the parent thread's context to the
 Hystrix managed thread. The `SEMAPHORE` isolation model is lighter-weight and
 should be used when you have a high-volume on your services and are running in
 an asynchronous I/O programming model (you are using an asynchronous I/O
 container such as Netty, not Tomcat).<br>
Fortunately, Hystrix and Spring Cloud offer a mechanism to propagate the parent
 thread's context to threads managed by the Hystrix Thread pool. This mechanism
 is called a `HystrixConcurrencyStrategy`.

## 6. Service routing with Spring Cloud and Zuul

Instead, all calls are routed through the service gateway, which acts as a
 single Policy Enforcement Point (PEP), and are then routed to a final
 destination. The use of a centralized PEP means that cross-cutting service
 concerns (static routing, dynamic routing, authentication and authorization,
 metric collection and logging) can be implemented in a single place without
 the individual development teams haveing to implement these concerns.<br>
Using the `@EnableZuulServer` annotation will create a Zuul Server that doesn't
 load any of the Zuul reverse proxy filters or use Netflix Eureka for service
 discovery. `@EnableZuulServer` is used when you want to build your own routing
 service and not use any Zuul prebuilt capabilities.<br>
Zuul at its heart is a reverse proxy. The Zuul proxy server is designed by
 default to work on the Spring products. As such, Zuul will automatically use
 Eureka to look up services by their service IDs and then use Netflix Ribbon to
 do client-side load balancing of requests from within Zuul.<br>
You can access the routes via `http://localhost:5555/routes` on the Zuul server.
 This will return a listing of all the mappings on your service.<br>
Zuul allows you to be more fine-grained by allowing you to explicitly define
 route mappings rather than relying solely on the automated routes created with
 the service's Eureka service ID.

```yaml
zuul:
  routes:
    organizationservice: /organization/**
    licensestatic:
      path: /licensestatic/**
      serviceId: licensestatic
ribbon:
  eureka:
    enalbed: false
licensestatic:
  ribbon:
    listOfServers: http://licenseservice-static1:8081, http://licenseservice-static2:8082
```

When you use automated route mapping where Zuul exposes the service based solely
 on the Eureka service ID, if no instances of the service are running, Zuul will
 not expose the route for the service.<br>
Eureka caches the location of the serivce instances locally and then checks with
 Eureka periodically for changes.<br>
The Spring Cloud sidecar allows you to register non-JVM services with a Eureka
 instance and then proxy yhem through Zuul.<br>
Zuul exposes a POST-based endpoint route `/refresh` that will cause it to reload
 its route configuration.<br>
The real power of Zuul comes into play when you want to write custom logic that
 will be applied against all the service calls flowing through the gateway.
 These application policies are considered *cross-cutting concerns* because you
 want them to be applied to all the services in your application without having
 to modify each service to implement them. While a servlet filter or Spring
 Aspect is localized to a specific serivce, using Zuul and Zuul filters allows
 you implement cross-cutting concerns across all the services being routed
 through Zuul.<br>
Zuul supports three types of filters: pre-filters, post filters and route
 filters. A pre-filter cannot redirect the user to a different endpoint or
 service. A route-level filter routes between two different versions of the
 same service so that a small percentage of calls to a service are routed to a
 new version of a service rather than the existing service. A Zuul route filter
 doesn't do an HTTP redirect, but will instead terminate the incoming HTTP
 request and then call the route on behalf of the original caller.<br>
When you want to add a value to the HTTP request headers, you use the
 RequestContext's `addZuulRequestHeader()` method. This method will maintain a
 separate map of HTTP headers that were added while a request was flowing
 through the filters with your Zuul server.<br>
This class is used to inject the correlation ID into any outgoing HTTP-based
 service requests being executed from a `RestTemplate` instance. To do this
 you're going to use a Spring Interceptor that's being injected into the
 `RestTemplate` class.

## 7. Securing your microservices

OAuth2 allows you to protect your REST-based services across these different
 scenarios through different authentication schemes called grants. The OAuth2
 specification has four types of grants: password, client credential,
 authorization code, implicit<br>
Your OAuth2 authentication service is going to be another Spring Boot
 service.<br>
The first dependency, `spring-cloud-security`, brings in the general Spring and
 Spring Cloud security libraries. The second dependency,
 `spring-security-oath2`, pulls in the Spring OAuth2 libraries. The
 `@EnableAuthorizationServer` annotation tells Spring Cloud that this service
 will be used as an OAuth2 service and to add several REST-based endpoints that
 will be used in the OAuth2 authentication and authorization processes.<br>
The payload returned from the `/auth/oauth/token` call contains five attributes:
 `access_token`, `token_type`, `refresh_token`, `expires_in` (Spring default 12
 hours), `scope`<br>
`/auth/user` endpoint response:

```json
{
  "user": {
    "password": null,
    "username": "john.carnell",
    "authorities": [
      {
        "authority": "ROLE_USER"
      }
    ],
    "accountNonExpired": true,
    "accountNonLocked": true,
    "credentialsNonExpired": true,
    "enabled": true
  },
  "authorities": [
    "ROLE_USER"
  ]
}
```

Any time you call an OAuth2 protected endpoint you need to pass along the OAuth2
 access token. To do this, always create an HTTP header called `Authorization`
 and with a value of `Bearer XXXXX`.<br>
The `@EnableResourceServer` annotation tells Spring Cloud and Spring Security
 that the service is a protected resource. The `@EnableResourceServer` enforces
 a filter that intercepts all incoming calls to the service, checks to see if
 there's an OAuth2 access token present in the incoming call's HTTP header, and
 then calls back to the callback URI defined in the
 `security.oauth2.resource.userInfoUri` to see if the token is valid. Once it
 knows the token is valid, the `@EnableResourceServer` annotation also applies
 any access control rules over who and what can access a service. To define
 access control rules, you need to extend a Spring
 `ResourceServerConfigurerAdapter` class and override the classes `configure()`
 method.

```java
@Override
public void configure(HttpSecurity http) throws Exception {
  http.authorizeRequests()
    .antMatcher(HttpMethod.DELETE, "/v1/organizations/**") // comma-separated list
    .hasRole("ADMIN").anyRequest().authenticated();
}
```

By default, Zuul won't forward sensitive HTTP headers such as `Cookie`,
 `Set-Cookie`, and `Authorization` to downstream services. To allow Zuul to
 propagate the "Authorization" HTTP header, you neeed to set the following
 configuration in your Zuul services gateway's application.yml or Spring Cloud
 Config data store: `zuul.sensitiveHeaders: Cookie,Set-Cookie`<br>
Zuul can automatically propagate downstream OAuth2 access tokens and authorize
 incoming requests against the OAuth2 service by using the `@EnableOAuth2Sso`
 annotation.<br>
Without Spring Security, you'd have to write a servlet filter to grab the HTTP
 header off the incoming licensing service call and then manually add it to
 every outbound service call in the licensing service. Spring OAuth2 provides a
 new Rest Template class that supports OAuth2 calls.<br>
OAuth2 is a token-based authentication framework, but ironically it doesn't
 provide any standards for how the tokens in its specification are to be
 defined. JWT (JavaScript Web Tokens) is an open standard (RFC-7519) that
 attempts to provide a standard structure for OAuth2 tokens. A JWT token is
 signed by the authenticating server that issues it. There's no need to call
 back to the authenticating service to validate the contents of the token
 because the signature of the token can be validated and the contents (such as
 the expiration time and the user information) can be inspected by the receiving
 microservice. However, to use and consume JWT tokens, your OAuth2
 authentication service and the services being protected by the authentication
 service must be configured in a different fashion.<br>
[Using JWT with Spring Security OAuth][using_jwt_with_spring_security_oauth]<br>
Notice that both the access_token and the refresh_token are now Base64-encoded
 strings. It's extremely important to understand that your JWT tokens are
 signed, but not encrypted.<br>
This is normally done via the `OAuth2RestTemplate` class, however, the
 `OAuth2RestTemplate` class doesn't propagate JWT-based tokens.<br>
Public microservices should be behind their own services gateway and have their
 own authentication service for performing OAuth2 authentication. The private
 zone should have its own services gateway and authentication services. Public
 API services should authenticate against the private zones authentication
 service.<br>
Many developers forget to lock down their outbound ports.

[using_jwt_with_spring_security_oauth]: http://www.baeldung.com/spring-security-oauth-jwt

## 8. Event-driven architecture with Spring Cloud Stream

JSON doesn't natively support versioning. However, you can use Apache Avro if
 you need versioning. Avro is a binary protocol that has versioning built into
 it. Spring Cloud Stream does support Apache Avro as a messaging protocol.<br>
A messaging-based architecture can be complex and requires the development team
 to pay close attention to several key things, including: message handling
 semantics, message visibility (correlation ID) and message choreography
 (difficult debugging).<br>
Written in Java, Kafka has become the de facto message bus for many cloud-based
 applications because it's highly reliable and scalable. Spring Cloud Stream
 also supports the use of RabbitMQ as a message bus.<br>
A channel name is always associated with a target queue name. The channel name
 is used in the code, which means that you can switch the queues the channel
 reads or writes from by changing the application's configuration, not the
 application's code.<br>
The `@EnableBinding(Source.class)` annotation tells Spring Cloud Stream that you
 want to bind the service to a message broker. Spring Cloud Stream has a default
 set of channels that can be configured to speak to a message broker. The
 `Source` interface is a Spring Cloud defined interface that exposes a single
 method called `output()` that returns a class of type `MessageChannel`. The
 `Source` interface is a convenient interface to use when your service only
 needs to publish to a single channel.<br>
The `send()` method takes a Spring `Message` class. You use a Spring helper
 class called `MessageBuilder` to take the contents of the model class and
 convert it to a Spring `Message` class.

```yaml
spring:
  stream:
    bindings:
      output: # in one application
        destination: orgChangeTopic
        content-type: application/json
      input: # in the other application
        destination: orgChangeTopic
        content-type: application/json
        group: licensingGroup # to guarantee process-once semantics for a service
      kafka:
        binder:
          zkNodes: localhost
          brokers: localhost
```

If you're going to pass state in your message, also make sure to include a
 date-time stamp or version number in your message so that the services
 consuming the data can inspect the data being passed and ensure that it's not
 older than the copy of the data they already have.<br>
Spring Cloud Stream will execute this
 `@StreamListener(Sink.INPUT) public void loggerSink(MyModel orgChange)` method
 every time a message is received off the input channel. The channel on the
 `Sink` interface is called `input`.<br>
The first thing you need to do is include the `spring-data-redis` dependencies,
 along with the `jedis` and `common-pools2` dependencies.<br>
Spring Data uses user-defined `@Repository` classes to provide a simple
 mechanism for a Java class to access your Postgres database without having to
 write low-level SQL queries.<br>
In the case of an `output` channel, the defined method will return a
 `MessageChannel` class instead of the `SubscribableChannel` class used with the
 `input` channel.

## 9. Distributed tracing with Spring Cloud Sleuth and Zipkin

Spring Cloud Sleuth is a Spring Cloud project that instruments your HTTP calls
 with correlation IDs and provides hooks that feed the trace data it's producing
 into OpenZipkin. Papertrail is a cloud-based service that allows you to
 aggregate logging data from multiple sources into single searchable database.
 Zipkin is an open source data-visualization tool that can show the flow of a
 transaction across multiple serivces. Zipkin allows you to break a transaction
 down into its component pieces and visually identify where there might be
 performance hotspots.<br>
You can add the correlation information to Spring's MDC (Mapped Diagnosis
 Context) logging so that the generated correlation ID is automatically logged
 by Spring Boots default SL4J and Logback implementation.<br>
`spring-cloud-starter-sleuth`<br>
Spring Cloud Sleuth will add four pieces of information to each log entry:
 application name of the service, trace ID, span ID, and whether trace data was
 sent to Zipkin. Span IDs (unique ID that represents part of the overall
 transaction) are particularly relevant when you integrate with Zipkin to
 visualize your transactions.<br>
options for log aggregation solutions for use with spring boot - ELK, Graylog
 (open source, on-premise), Splunk (on-premise, cloud-based), Simo Logic
 (cloud-based, requires a corporate work account to signup), Papertrail
 (cloud-based)<br>
configure a Papertrail syslog connector and define a [Logspout][logspout] Docker
 container to capture standard out from all the Docker containers. In Docker,
 all containers write their standard out to an internal filesystem called
 Docker.sock. A Logspout Docker container listens to Docker.sock and writes
 whatever goes to standard output to a remote syslog location.

```yaml
# docker/common/docker-compose.yml
logspout:
  image: gliderlabs/logspout
  command: syslog://logs5.papertrailapp.com:21218
  volumes:
    - /var/run/docker.sock:/var/run/docker.sock
```

Docker 1.6+ do allow you to define alternative logging drivers to write the
 stdout/stderr messages written from each container. Why did I choose Logspout
 instead of using the standard Docker log driver? The ability to send log data
 to multiple endpoints at once, a centralized location for filtering, custom
 HTTP routes that let applications write log information, and integration with
 protocols beyond syslog.<br>
Learn a query language of your logging tool.<br>
Spring Cloud Sleuth team believes that returning any of the tracing data can be
 a potential security issue.<br>
The Tracer class is entry point to access trace and span ID information.<br>
(Zipkin client dependency) `spring-cloud-starter-sleuth`,
 `spring-cloud-sleuth-core`, `spring-cloud-sleuth-zipkin`, (Zipkin server
 dependency) `zipkin-server`, `zipkin-autoconfigure-ui`<br>
From a functionality persepctive, there's no difference in Zipkin behavior if
 you use HTTP, RabbitMQ, or Kafka.<br>
While Zipkin is a Spring-Boot-based project, the `@EnableZipkinServer` is not a
 Spring Cloud annotation. It's an annotation that's part of the Zipkin project.
 The Spring Cloud team did write the `@EnableZipkinStreamServer` annotation as
 part of Spring Cloud Sleuth. The `@EnableZipkinStreamServer` annotation
 simplifies the use of Zipkin with RabbitMQ and Kafka. If you use the
 `@EnableZipkinServer` annotation and the Zipkin server is unavailable, the
 trace data that would have been sent by the service to Zipkin will be lost.<br>
Zipkin supports four different back end data stores: in-memory data, MySQL,
 Cassandra, and Elasticsearch.<br>
By default, Zipkin will only write 10% of all transactions to the Zipkin server.

[logspout]: https://github.com/gliderlabs/logspout

## 10. Deploying your microservices

In Amazon's cloud, an Amazon-managed Redis server can only be accessed by
 servers that are in the same VPC as the Redis server.<br>
integration tests (with mocked or stubbed) vs platform tests<br>
This killing and resurrection of a new server was termed
 [Phoenix Server][phoenix_server] by Martin Fowler. Randomly killing and
 restarting servers quickly exposes situations where you have state in your
 services or infrastructure.<br>
To add an encrypted environment variable, you must encrypt the environment
 variable using the `travis encrypt DOCKER_USERNAME=... --add env.global`
 command line tool on your desk in the project directory where you have your
 source code. Encrypted variables are only good for the single GitHub
 repository they're encrypted in and Travis is building against.<br>
This
 `BUILD_NAME=chapter10-$TRAVIS_BRANCH-$(date -u "+%Y%m%d%H%M%S")-$TRAVIS_BUILD_NUMBER`
 will be used to tag your source code in GitHub and your Docker image when it's
 pushed to the Docker hub repository.

[phoenix_server]: https://martinfowler.com/bliki/PhoenixServer.html

## appendix A. Running a cloud on your desktop

```shell
getPort() {
    echo $1 | cut -d : -f 3 | xargs basename
}

while ! `nc -z database $(getPort $DATABASESERVER_PORT)`; do sleep 3; done
```

Each service being launched by Docker Compose has a label applied to it. This
 will become the DNS entry for the Docker instance. Docker Compose will first
 try to find the target image to be started in the local Docker repository. If
 it can't find it, it will check the central Docker hub.<br>
`docker-compose -f docker/common/docker-compose.yml up -d` →
 `docker-compose -f docker/common/docker-compose.yml logs -f licensingservice`

## appendix B. OAuth2 grant types

Password grants
* EagleEye passes the user credentials, along with the application
  name/application secret key, directly to the EagleEye OAuth2 service.
* Both the application making the request for a server and the services are
  trusted and are owned by the same organization.

Client credential grants
* The client credentials grant is typically used when an application needs to
  access an OAuth2 protected resource, but no human being is involved in the
  transaction. With the client credentials grant type, the OAuth2 server only
  authenticates based on application name and the secret key provided by the
  owner of the resource.

Authorization code grants
* The authorization code grant is by far the most complicated of the OAuth2
  grants, but it's also the most common flow used because it allows different
  applications from different vendors to share data and services without having
  to expose a user's credentials across multiple applications.
* As part of the registration process, they'll also provide a callback URL back
  to their Salesforce-based application. This callback URL is a Salesforce URL
  that will be called after the EagleEye OAuth2 server has authenticated the
  user's EagleEye credentials.
* This authorization code isn't an OAuth2 access token.
* Once the authorization code has been stored, the custom Salesforce application
  can present the Salesforce application the secret key they generated during
  the registration process and the authorization code back to EagleEye OAuth2
  server. The EagleEye OAuth2 server will validate that the authorization code
  is valid and then return back an OAuth2 token to the custom Salesforce
  application.
* Even though the user is logged into Salesforce and they're accessing EagleEye
  data, at no time were the user's EagleEye credentials directly exposed to
  Salesforce.

Implicit grant
* With an implicit grant, you're usually working with a pure JavaScript
  application running completely inside of the browser.
* The owner of the JavaScript application has registered the application with
  the EagleEye OAuth2 server. They're provided an application name and also a
  callback URL that will be redirected with the OAuth2 access token for the
  user.
* If the user successfully authenticates, the EagleEye OAuth2 service won't
  return a token, but instead redirect the user back to a page the owner of the
  JavaScript application registered. In the URL being redirected back to, the
  OAuth2 access token will be passed as a query parameter by the OAuth2
  authentication service.
* The application will take the incoming request and run a JavaScript script
  that will parse the OAuth2 access token and store it (usually as a cookie).
* The implicit grant is the only grant type where the OAuth2 access token is
  directly exposed to a public client (web browser). With an authorization code,
  the returned OAuth2 token is never directly exposed to the user's browser.
* OAuth2 tokens generated by the implicit grant are more vulnerable to attack
  and misuse because the tokens are made available to the browser.
* The implicit grant type OAuth2 tokens should be short-lived (1-2 hours).
  Because the OAuth2 access token is stored in the browser, the OAuth2 spec (and
  Spring Cloud security) doesn't support the concept of a refresh token in which
  a token can be automatically renewed.

In most of the OAuth2 grant flows, the OAuth2 server will issue both an access
 token and a refresh token.

