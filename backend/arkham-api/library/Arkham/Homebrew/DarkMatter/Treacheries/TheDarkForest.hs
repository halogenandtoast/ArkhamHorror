module Arkham.Homebrew.DarkMatter.Treacheries.TheDarkForest (theDarkForest) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), inThreatAreaGets)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype TheDarkForest = TheDarkForest TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDarkForest :: TreacheryCard TheDarkForest
theDarkForest = treachery TheDarkForest Cards.theDarkForest

{- | "As an additional cost to trigger an [action] ability, draw the top two cards
of the encounter deck. Then, discard this card."
-}
instance HasModifiersFor TheDarkForest where
  getModifiersFor (TheDarkForest a) =
    inThreatAreaGets a [AdditionalCostToPerformAction (IsAction #activate) (DrawEncounterCardsCost 2)]

{- | The "Then, discard this card." half of the additional cost, plus
"[reaction] At the end of your turn: Test [agility] (4). If you succeed, discard
this card."
-}
instance HasAbilities TheDarkForest where
  getAbilities (TheDarkForest a) =
    [ mkAbility a 1 $ SilentForcedAbility $ ActivateAbility #after You AnyAbility
    , restricted a 2 Here $ freeReaction $ TurnEnds #when You
    ]

instance RunMessage TheDarkForest where
  runMessage msg t@(TheDarkForest attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid attrs attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 4)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> TheDarkForest <$> liftRunMessage msg attrs
