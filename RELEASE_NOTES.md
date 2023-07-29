## 0.10.0
* breaking: wrap all modules in a `DiscordBotExtensions` namespace
* breaking: remove `Shared` namespace
* breaking: move `IMongoCollection.bulkWriteEmpty` to `MongoCollection`
* breaking: move `Types.IMongoCollection.isEmpty` to `Db.MongoCollection`
* breaking: move `Types.StandartDiscordEmoji` to `StandardDiscordEmoji`
* breaking: move `Types.tryGetEnvironmentVariable` and `Types.getEnvironmentVariable` in `EnvironmentExt` module
* breaking: move `BotModule.botEventId`, `BotModule.empty`, `BotModule.bindToClientsEvents` in `BotModule.BotModule`
* breaking: move `BotModule.BotModule` to `BotModule`
* breaking: rename `CommandParser` to `PrefixCommandParser`
* breaking: pull out `PrefixCommandParser.Cmd` type from module and rename it to `PrefixCommandParser`
* breaking: make members of type `PrefixCommandParser` require qualified access
* breaking: move `Parser<'a>` to `PrefixCommandParser` module
* breaking: move `Types.MessagePath` to `DiscordMessage` namespace
* breaking: move `Types.getGuildMember` to `DiscordGuild.getMember`
* breaking: swap args in `DiscordGuild.getMember`

## 0.9.0
* feat: add MVC helper

## 0.8.0
* improvement `handleForms`

## 0.7.0
* feat: add `Interaction.handleForms`

## 0.6.0
* feat: typing started handler in BotModule

## 0.5.2
* fix: run schedulers many times

## 0.5.1
* get bot module binder from ready event

## 0.5.0
* feat: add command parser

## 0.4.1
* db: fix: Must contain at least 1 request. (Parameter 'requests')

## 0.4.0
* feat: InteractionCommand

## 0.3.0
* feat: add Types.ChannelPath

## 0.2.0
* update DSharpPlus to 4.3
* fix: StandardDiscordEmojis resource
* init
