module Arkham.Location.Cards.JardinesDeLaTropical (jardinesDeLaTropical) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype JardinesDeLaTropical = JardinesDeLaTropical LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jardinesDeLaTropical :: LocationCard JardinesDeLaTropical
jardinesDeLaTropical = symbolLabel $ location JardinesDeLaTropical Cards.jardinesDeLaTropical 3 (PerPlayer 1)

instance HasAbilities JardinesDeLaTropical where
  getAbilities (JardinesDeLaTropical a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> exists HollowedCard)
          $ freeReaction
          $ SkillTestResult #after You (WhileInvestigating (be a)) (SuccessResult $ atLeast 3)
      , restricted a 2 (Here <> youExist (HandWith $ HasCard NonWeakness))
          $ forced
          $ SkillTestResult #after You (WhileInvestigating (be a)) (FailureResult $ atLeast 3)
      ]

instance RunMessage JardinesDeLaTropical where
  runMessage msg l@(JardinesDeLaTropical attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- getInvestigators
      hollowed <- for investigators \iid' -> do
        mods <- getModifiers iid'
        hollows <- traverse fetchCard [cardId | Hollow cardId <- mods]
        pure (iid', hollows)
      focusCards (concatMap snd hollowed) do
        chooseOneM iid do
          for_ hollowed \(iid', cards) -> do
            targets cards $ drawCard iid'
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      cards <- select $ basic NonWeakness <> oneOf [inHandOf NotForPlay iid, inPlayAreaOf iid]
      focusCards cards $ chooseTargetM iid cards $ hollow iid
      pure l
    _ -> JardinesDeLaTropical <$> liftRunMessage msg attrs
