module Arkham.Homebrew.AgesUnwound.Locations.Shanghai (shanghai) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)

newtype Shanghai = Shanghai LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shanghai :: LocationCard Shanghai
shanghai = symbolLabel $ location Shanghai Cards.shanghai 4 (PerPlayer 1)

-- | "__Move__ to San Francisco or Sydney."
destinations :: LocationMatcher
destinations = mapOneOf locationIs [Cards.sanFrancisco, Cards.sydney]

{- | "__Forced__ - After you fail a skill test while investigating Shanghai: Lose
2 resources. / [action][action]: __Move__ to San Francisco or Sydney. / [action]
Spend 3 resources: Gain 1 clue from the token pool. (Limit once per round.)"

The clue ability's printed limit names no group, so it is a limit per
investigator.
-}
instance HasAbilities Shanghai where
  getAbilities (Shanghai a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ SkillTestResult #after You (WhileInvestigating $ be a) #failure
      , campaignI18n
          $ withI18nTooltip "shanghai.move"
          $ restricted a 2 (Here <> exists destinations)
          $ ActionAbility #move Nothing (ActionCost 2)
      , campaignI18n
          $ withI18nTooltip "shanghai.gainClue"
          $ playerLimit PerRound
          $ restricted a 3 Here
          $ actionAbilityWithCost (ResourceCost 3)
      ]

instance RunMessage Shanghai where
  runMessage msg l@(Shanghai attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseResources iid (attrs.ability 1) 2
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      lids <- select destinations
      chooseTargetM iid lids $ moveTo (attrs.ability 2) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      gainClues iid (attrs.ability 3) 1
      pure l
    _ -> Shanghai <$> liftRunMessage msg attrs
