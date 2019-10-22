# love-jaredweakly-com
If you're reading this and you're not Chloe, don't worry, I love you too. (Just not this much)

TODO:

1. Make "light" and "dark" sufficiently contrasting from the primary color triplets
2. Tweak and maybe get rid of some gradient options
    - The triple color with the stripe down the middle just doesn't look good
3. Make blur text have some sort of shadow or thing to make it stand out better
    - Inset transparency?
4. Add very subtle blurry bg to the shadow text template
5. Actually write and design a multi-sentence template.
6. Can I make a serverless.yml file for this thing? That would be slick.
7. As an excuse to fuck around with polysemy, pull out handler into different interpretations
    - handler (duh)
    - pure, for all the tests I'm never going to write
    - CLI. Maybe use brick, maybe just pipe it to cowsay ¯\\\_(ツ)\_/¯
    - HTML in stdout (muh streamz™)

On Arch Linux, this requires having the libgmp-static package from the aur installed. Yay static linking.

After reading the source code, I've realized that the correct handler for aws is "src/Lib.handler".
In retrospect, this was visible in the docs, just not explicitly pointed out.
Note to self: make a PR to the aws runtime library to improve the "hello world" example documentation.

```sh
aws lambda update-function-configuration --function-name test --handler src/Lib.handler
```

## This lambda function will run on love.jaredweakly.com
Upon navigating to the page:

* Picks a random quote
* Picks a random theme
* Varies the theme
* Displays the result

### Quotes
* Why are you like this?
* You're my favorito, baberrito
* Neato mosquito, baberrito
* You're my favorite
* Did you know that... You're my favorite?
* Did you  know that you're my mostest favoritest in the whole wide world?
* "Hey that's mine" "I know. That's why I like touching it so much"
* "Hey guess what?" "What" "I love you so much"
* Snug me
* I'm a big fan of the snugs
* "Snug me" "I'm a big fan of the snugs"
* I love you
* ily
* ilysm <3
* Exploding hearts animation
* You are ... The love of my life (and my wife)
* You so cute
* I love you the mostest
* You make my heart feel melty
* Hey I luff you


### Vows
* I promise that I will always say "maybe" when you ask about puppies.
    - Because it won't just be one.
* I promise I won't be mad when it's our dog
    - But my dog poop.
* I promise to always be the dance to your song.
    - Especially in the middle of Target's clearance section.
* I promise to always be ready for frozen yogurt.
    - You'll think I'm reading your mind, but it's hard to guess wrong when the odds are 95 to 1.
* I promise to not let expectations get in the way of every-day-ness.
* I promise to take this life as it comes; together with you
* I promise to choose you, to emote with you, to live with you, to commit to you, to journey with you, struggle with you, and learn with you.
* I promise to love you.
