module Arkham.Homebrew.DarkMatter.Treacheries.ComeCLOSER (comeCLOSER) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Treachery.Import.Lifted

newtype ComeCLOSER = ComeCLOSER TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

comeCLOSER :: TreacheryCard ComeCLOSER
comeCLOSER = treachery ComeCLOSER Cards.comeCLOSER

{- | "Revelation - Put COME CLOSER into play in your threat area.
Forced - At the end of your turn, if you are not at The Boogeyman's location:
Discard COME CLOSER and test [willpower] (3). If you fail, move one location
towards The Boogeyman."
-}
instance HasAbilities ComeCLOSER where
  getAbilities (ComeCLOSER a) =
    [ skillTestAbility
        $ restricted
          a
          1
          (InThreatAreaOf You <> not_ (youExist $ at_ (LocationWithEnemy $ enemyIs Enemies.theBOOGEYMAN)))
        $ forced
        $ TurnEnds #when You
    ]

instance RunMessage ComeCLOSER where
  runMessage msg t@(ComeCLOSER attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      selectOne (enemyIs Enemies.theBOOGEYMAN) >>= traverse_ \boogeyman ->
        withLocationOf boogeyman \lid -> moveTowards (attrs.ability 1) iid lid
      pure t
    _ -> ComeCLOSER <$> liftRunMessage msg attrs
