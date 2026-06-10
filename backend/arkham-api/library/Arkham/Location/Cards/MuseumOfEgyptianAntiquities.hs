module Arkham.Location.Cards.MuseumOfEgyptianAntiquities (museumOfEgyptianAntiquities) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype MuseumOfEgyptianAntiquities = MuseumOfEgyptianAntiquities LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

museumOfEgyptianAntiquities :: LocationCard MuseumOfEgyptianAntiquities
museumOfEgyptianAntiquities =
  symbolLabel $ location MuseumOfEgyptianAntiquities Cards.museumOfEgyptianAntiquities 1 (PerPlayer 1)

instance HasAbilities MuseumOfEgyptianAntiquities where
  getAbilities (MuseumOfEgyptianAntiquities a) =
    extendRevealed
      a
      [ skillTestAbility $ restricted a 1 Here actionAbility
      , mkAbility a 2
          $ forced
          $ SkillTestResult
            #after
            You
            (WhileInvestigating $ be a)
            (oneOf [SuccessResult $ atMost 1, FailureResult AnyValue])
      ]

instance RunMessage MuseumOfEgyptianAntiquities where
  runMessage msg l@(MuseumOfEgyptianAntiquities attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 5)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember DiscoveredAnAncientTablet
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      setActions iid (attrs.ability 2) 0
      endYourTurn iid
      pure l
    _ -> MuseumOfEgyptianAntiquities <$> liftRunMessage msg attrs
