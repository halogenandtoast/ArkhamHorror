module Arkham.Homebrew.TheMasqueOfTheRedDeath.Locations.GreenChamber (greenChamber) where

import Arkham.Ability
import Arkham.ChaosToken (pattern NegativeModifier)
import Arkham.ChaosToken.Types (ChaosTokenValue (ChaosTokenValue))
import Arkham.Helpers.History (HistoryField (HistoryCluesDiscovered), getHistoryField)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (
  chamberToll,
  describedSkullEffect,
  tollDoomMayAdvanceAgenda,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GreenChamber = GreenChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greenChamber :: LocationCard GreenChamber
greenChamber =
  locationWith GreenChamber Cards.greenChamber 1 (PerPlayer 2)
    $ costToEnterUnrevealedL
    .~ chamberToll

instance HasAbilities GreenChamber where
  getAbilities (GreenChamber a) =
    extendRevealed
      a
      -- "[skull]: -1. -3 instead if you have discovered 1 or more clues this round."
      [ describedSkullEffect 0 "-1. -3 instead if you have discovered 1 or more clues this round." a 1
      , -- "[reaction] After you deal damage to an enemy in excess of its remaining
        -- health: Discover 1 clue at Green Chamber. (Limit once per round.)"
        playerLimit PerRound
          $ restricted a 2 Here
          $ freeReaction
          $ EnemyDealtExcessDamage #after AnyDamageEffect AnyEnemy (SourceUsedBy You)
      ]

instance RunMessage GreenChamber where
  runMessage msg l@(GreenChamber attrs) = runQueueT do
    tollDoomMayAdvanceAgenda attrs msg
    case msg of
      UseThisAbility iid (isSource attrs -> True) 1 -> do
        discovered <- sum . toList <$> getHistoryField #round iid HistoryCluesDiscovered
        let n = if discovered > 0 then 3 else 1
        withSkillTest \sid ->
          skillTestModifier sid (attrs.ability 1) sid
            $ AddChaosTokenValue (ChaosTokenValue #skull (NegativeModifier n))
        pure l
      UseThisAbility iid (isSource attrs -> True) 2 -> do
        discoverAt NotInvestigate iid (attrs.ability 2) 1 attrs
        pure l
      _ -> GreenChamber <$> liftRunMessage msg attrs
