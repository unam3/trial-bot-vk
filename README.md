# VK echo-bot: trial-bot-vk

## Installation and Usage

To use bot one need to install [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) and then run in terminal from project directory:

```
stack build
```


All configuration is done by five parameters in trial-bot-vk launch command (run from project directory):

```
stack exec trial-bot-vk-exe token groupId helpMsg repeatMsg echoRepeatNumberStr
```
where
- token — [community token](https://vk.com/dev/access_token?f=2.%20Community%20Token);
- groupId — community ID;
- helpMsg — string, which bot will send to the `/help` message;
- repeatMsg — string, which bot will send to the `/repeat` message with currently set repeat number;
- echoRepeatNumberStr - number from 1 to 5 (inclusive) of repeats to one message.

Example of launch command in Linux environment:

```
stack exec trial-bot-vk-exe "123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11" "123456789" "help msg" "repeat msg" 1
```


## Constraints

It's assumed that bot will be used only in one private chat.
Only latest text message will be processed.
