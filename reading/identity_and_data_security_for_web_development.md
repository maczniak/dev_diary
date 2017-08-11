# [Identity & Data Security for Web Development][homepage] by Jonathan LeBlanc and Tim Messerschmidt, O'Reilly (2016)

of PayPal and Braintree<br>
A special mention goes to [Danese Cooper][danese_cooper], PayPal's Head of Open
 Source, who strongly encouraged me to write down my thoughts beyond blog posts.

[homepage]: http://shop.oreilly.com/product/0636920044376.do
[danese_cooper]: https://en.wikipedia.org/wiki/Danese_Cooper

## 1. Introduction

In addition, following this incident, Slack introduced two-factor authentication
 for users, as well as a password kill switch for team owners that automatically
 logged out all users, on all devices, and forced them to create a new password.
 Data encryption isn't always about trying to prevent data from being stolen;
 it's meant to slow down hackers long enough to make it infeasible for them to
 decrypt massive amounts of data, or to delay them until you can take
 appropriate action.<br>
A popular example is the password-management application Keychain that was
 introduced with Mac OS 8.6. Keychain is deeply integrated into OS X and
 nowadays in iOS (via iCloud) and allows for storing various types of data
 including credit cards, passwords, and private keys.<br>
Typically, if you are using passphrases, a good level of entropy
 (*log*<sub>2</sub>(*b*<sup>*l*</sup>)) to have at minimum is 36.86 bits, which
 coincides with the average entropy level of 3 random words selected from a list
 of 5,000 possible unique words. According to "A Large-Scale Study of Web
 Password Habits," by Dinei Florencio and Cormac Herley of Microsoft Research,
 the entropy level of the average password is estimated to be 40.54 bits.<br>
PBKDF2 (Password-Based Key Derivation Function 2, by RSA Laboratories, baked
 into the Node crypto library), bcrypt (Belgian Fundamental Research in
 Cryptology and Information Security, based on the blowfish cipher), scrypt
 (require high amounts of memory)<br>
In all seriousness, the impact of social engineering is often completely
 underestimated or even ignored.

## 2. Password Encryption, Hashing, and Salting

Understand that database encryption is absolutely needed, even though in 99% of
 organizations, this is simply not done.<br>
Data federation is another method to help prevent unwanted access in the case of
 an application with global reach and data storage. The purpose of this strategy
 is to maintain distinct database systems in the regions where the personal
 information is needed.<br>
Beyond the user data, sensitive financial information such as credit card data
 can also be offloaded, typically to the payment provider through a system such
 as a credit card vault. In addition to the security benefits of not having to
 host that data yourself, you don't incur the implications of having to
 implement all standards for PCI DSS compliance, as required when hosting
 payment information for customers.<br>
[Authy][authy] (two-factor authentication app)

```javascript
/*
<form method="post" action="/capture">
    <div class="g-recaptcha" data-sitekey="6Lc2FxgTAAAXXXXXXXXXXXX"></div>
    <input type="submit" value="Submit">
</form>

{
  "success": true
}
{
  "success": false
  "error-codes": [
    "missing-input-response"
  ]
}
*/
app.post('/capture', function (req, res){
    var response = req.body['g-recaptcha-response'];

    var verify_data = querystring.stringify({
        'secret': 'YOUR SECRET KEY',
        'response': response
    });

    var verify_options = {
        host: 'google.com',
        path: '/recaptcha/api/siteverify',
        method: 'POST',
        headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
            'Content-Length': verify_data.length
        }
    };

    var post_req = https.request(verify_options, function(result){
        result.setEncoding('utf8');
        result.on('data', function (verification){
            console.log(verification);
        });
    });

    post_req.write(verify_data);
    post_req.end();
});
```

One important aspect is required for rainbow tables to function, and that's
 called a *reduction function*. In short, the purpose of a reduction function is
 to take a given hash and run the algorithm to generate the next possible
 plain-text password for us. We store only the first plain-text password, and
 the last hash in the 10,000 word/hash chain.<br>
One general rule of thumb is for the salt to the same size as the output of the
 hash function used, at minimum. We don't need to obfuscate or encrypt the salt.

[authy]: https://authy.com/

## 3. Identity Security Fundamentals

three types of identity - social identity, concrete identity, thin identity (or
 even *no* identity, like Firebase phone autentication)<br>
Often applied through single sign-on--known as SSO--Federated Identity
 Management (FIM or FIdM) is the practice of using a set of identity attributes
 across multiple systems or organizations. While SSO is a way of using the same
 credentials across multiple sites and applications, FIM shifts the verfication
 of credentials toward the identity provider.
build up trust zones - browser fingerprinting (like EFF
 [Panopticlick][panopticalick]), localtion-based tracking, device
 fingerprinting, connected devices (Bluetooth), GPS, WiFi, biometric factors,
 camera and other sensors<br>
The EFF noticed that the distribution of entropy observed on a tested browser is
 typically around 18.1 bits (user agent 10.0, plug-ins 15.4 (by
 [PluginDetect][plugindetect]), fonts 13.9, video 4.83, supercookies 2.12, http
 accept header 6.09, time zone 3.04, cookiesenabled 0.353). This means that if a
 browser was chosen at random, at best we would expect that only 1 in 286,777
 other browsers would share the same fingerprint.

[panopticalick]: https://panopticlick.eff.org/
[plugindetect]: http://www.pinlady.net/PluginDetect/

## 4. Securing the Login with OAuth 2 and OpenID Connect

(three-legged) [OAuth][oauth] 1.0a authorization flow
1. Consumer: retrieve a Request Token
1. Service Provider: grant Request Token
1. Consumer: direct user to the Service Provider in order to sign in
1. Service Provider: obtain authorization
1. Service Provider: redirect the user to the Consumer
1. Consumer: request an Access Token
1. Service Provider: grant Access Token
1. Consumer: use Access Token to access protected resources

By implementing OAuth, a potentially insecure password is replaced by an opaque
 token that can be revoked by the application and the End User. An Access Token
 in OAuth 1.0 is valid until it is revoked manually. (Refresh Tokens (optional)
 in OAuth 2.0 allow refreshing Access Tokens after they are consumed or
 expired.) An alternative flow, known as *two-legged OAuth*, skips obtaining the
 user's authorization because no user data is being requested or involved. The
 two-legged OAuth flow can be used as a replacement for traditional basic
 authentication.<br>
OAuth 2.0 (for more simplicity and to accommodate web applications, native
 applications, and even interfaceless consumers)
* Access Tokens are now subject to a time to live (TTL)/expiry time
* no more client-side cryptography (i.e., must use TLS)
* different flows to accommodate different authentication scenarios

1. Consumer: direct user to the Service Provider in order to sign in
1. Service Provider: obtain authorization
1. Service Provider: redirect the user to the Consumer
1. Consumer: use Authorization Code to request Access Token
1. Service Provider: grant Access Token
1. Consumer: use Access Token to access protected resources

[OpenID Connect][openid_connect] is a standard issued by the OpenID Foundation
 in February 2014 and resembles an extra layer on the top of the OAuth 2.0 core
 that handles user authentication in a standardized REST-like manner.<br>
[Random Number Generation in JavaScript][random_number_generation_in_javascript],
 David Bau's [ARC4 implementation][arc4_impl] (an example of a full featured,
 robust generator)<br>

[oauth]: https://oauth.net/
[openid_connect]: http://openid.net/specs/openid-connect-core-1_0.html
[random_number_generation_in_javascript]: https://bocoup.com/blog/random-numbers
[arc4_impl]: http://davidbau.com/archives/2010/01/30/random_seeds_coded_hints_and_quintillions.html

## 5. Alternate Methods of Identification

## 6. Hardening Web Applications

## 7. Data Transmission Security

## A. GitHub Repositories

## B. Technical Preconditions and Requirements

