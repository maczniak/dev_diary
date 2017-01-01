# [Just Spring][homepage] by Madhusudhan Konda, O'Reilly (2011)

[homepage]: http://shop.oreilly.com/product/0636920020394.do

## 1. Spring Basics

`org.springframework.beans.factory.config.PropertyPlaceholderConfigurer`

## 2. Spring Beans

Beans are object instances that would be created by the spring framework by
looking at their class definitions that basically form the configuration
metadata.<br>
Alternatively, one could provide the metadataas Annotation or Java
Configuration.<br>
`BeanNameAware` or `BeanFactoryAware` -> `BeanPostProcessor`'s
`postProcessBeforeInitialization` and `postProcessAfterInitialization`
-> `init-method` (`destroy-method`) -> `InitializingBean`'s `afterPropertiesSet`
(`DisposableBean`'s `destroy)<br>
the default scope is always singleton. singleton per context or container
(neither per process nor per class loader)

## 3. Advanced Concepts

A container is basically a pool of beans created in a memory space by the
framework when the application starts up. An API provided by the framework
exposes methods to query the container beans.<br>
Note that beans are loaded lazily into the container. This strategy works the
same for any container in Spring, except loading Singletons in the
ApplicationContext container.<br>
Spring containers primarily fall into two categories: Bean Factories and
Application Contexts. The `BeanFactory` is usually preferred in small device
applications such as mobile phones, etc., where the resources are limited.
(`addBeanPostProcessor()` is necessary.)<br>
Spring's event handling is *single-threaded*. It is primarily synchronous in
nature. That is, if an event is published, until and unless all the receivers
get the message, the processes are blocked and the flow will not continue.<br>
autowire - byName, byType, constructor

## 4. Spring JMS

`MessageCreator`, `SessionCallback`, `ProducerCallback`

```xml
<bean id="tradePublisher" class="com.oreilly.justspring.jms.TradePublisher">
  <property name="jmsTemplate" ref="jmsTemplate"/>
  <property name="destinationName" value="justspring.jms.testQueue"/>
</bean>
<bean id="jmsTemplate" class="org.springframework.jms.core.JmsTemplate">
  <property name="connectionFactory" ref="connectionFactory" />
  <property name="defaultDestination" ref="defaultDestination" />
  <property name="pubSubDomain" value="true" />
  <property name="receiveTimeout" value="2000" />
  <property name="messageConverter" ref="tradeMessageConverter" />
</bean>
<bean id="connectionFactory" class="org.apache.activemq.ActiveMQConnectionFactory">
  <property name="brokerURL">
    <value>tcp://localhost:61616</value>
  </property>
</bean>
<bean id="defaultDestination" class="org.apache.activemq.command.ActiveMQQueue">
  <constuctor-arg value="justspring.jms.testQueue2" />
</bean>
<bean id="defaultListenerContainer" class="org.springframework.jms.listener.DefaultMessageListenerContainer">
  <property name="connectionFactory" ref="connectionFactory" />
  <property name="destination" ref="defaultDestination" />
  <property name="messageListener" ref="tradeMessageListener" />
</bean>
<bean id="tradeMessageListener" class="com.oreilly.justspring.jms.consumer.TradeMessageListener" />
<bean id="tradeMessageConverter" class="com.oreilly.justspring.jms.converter.TradeMessageConverter"/>
```
## 5. Spring Data

database-specific exceptions are wrapped by Spring's Runtime Exceptions.<br>
`PreparedStatementCallback`, `RowCallbackHandler`, `CallableStatementCallback`<br>
`JdbcTemplate`'s `queryForXXX`/`query`/`update`, `RowMapper` interface's `mapRow` and `RowMapperResultSetExtractor`

```xml
<bean id="movieDataSource" class="org.apache.commons.dbcp.BasicDataSource" destory-method="close">
  <property name="driverClassName" value="${jdbc.driver}"/>
  <property name="url" value="${jdbc.url}"/>
  <property name="username" value="${jdbc.username}"/>
  <property name="password" value="${jdbc.password}"/>
</bean>
<bean id="movieDao" class="com.oreilly.justspring.data.dao.MovieDAO" destory-method="close">
  <property name="jdbcTemplate" ref="jdbcTemplate"/>
</bean>
<bean id="jdbcTemplate class="org.springframework.jdbc.core.JdbcTemplate">
  <property name="dataSource" ref="movieDataSource"/>
</bean>
```

```xml
<!-- Movie.hbm.xml -->
<hibernate-mapping>
  <class name="com.oreilly.justspring.springdata.domain.Movie" table="MOVIES">
  <id name="id" column="ID">
    <generator class="assigned"/>
  </id>
  <property name="title" column="TITLE"/>
  <property name="genre" column="GENRE"/>
  <property name="synopsis" column="SYNOPSIS"/>
  </class>
</hibernate-mapping>
```

```xml
<bean id="sessionFactory" class="org.springframework.orm.hibernate.LocalSessionFactoryBean">
  <property name="dataSource" ref="movieDataSource"/>
  <property name="hibernateProperties">
    <props>
      <prop key="hibernate.dialect">net.sf.hibernate.dialect.MySQLDialect</prop>
      <prop key="hibernate.show_sql">false</prop>
    </props>
  </property>
  <property name="mappingResources">
    <list>
      <value>Movie.hbm.xml</value>
    </list>
  </property>
  <bean id="movieDataSource" class="org.apache.commons.dbcp.BasicDataSource" destroy-method="close">
    ...
  </bean>
</bean>
<bean id="hibernateTemplate" class="org.springframework.orm.hibernate.HibernateTemplate">
  <property name="sessionFacotry" ref="sessionFactory"/>
</bean>
<bean id="movieDao" class="com.oreilly.justspring.data.dao.MovieDAO">
  <property name="hibernateTemplate" ref="hibernateTemplate"/>
</bean>
```

