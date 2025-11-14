module Arkham.Location.Cards.RelicRoomSanctumOfFortune (relicRoomSanctumOfFortune) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Campaigns.TheScarletKeys.Key.Types (Field (..))
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.Query (getJustLocationByName)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Token (countTokens)

newtype RelicRoomSanctumOfFortune = RelicRoomSanctumOfFortune LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicRoomSanctumOfFortune :: LocationCard RelicRoomSanctumOfFortune
relicRoomSanctumOfFortune = symbolLabel $ location RelicRoomSanctumOfFortune Cards.relicRoomSanctumOfFortune 4 (PerPlayer 1)

instance HasModifiersFor RelicRoomSanctumOfFortune where
  getModifiersFor (RelicRoomSanctumOfFortune a) = do
    modifySelfWhen a a.unrevealed [Blocked]

instance HasAbilities RelicRoomSanctumOfFortune where
  getAbilities (RelicRoomSanctumOfFortune a) =
    if a.unrevealed
      then
        extendUnrevealed1 a
          $ scenarioI18n
          $ withI18nTooltip "relicRoom.reveal"
          $ restricted (proxied (LocationMatcherSource "Vault Door") a) 1 (OnLocation "Vault Door")
          $ actionAbilityWithCost
          $ CostIfRemembered FoundAbarransSigil Free
          $ DamageCost (a.ability 1) YouTarget 1
          <> HorrorCost (a.ability 1) YouTarget 1
      else
        extendRevealed
          a
          [ mkAbility a 1 $ forced $ RevealLocation #when Anyone (be a)
          , restricted a 2 (Here <> thisExists a LocationWithoutClues) actionAbility
          ]

instance RunMessage RelicRoomSanctumOfFortune where
  runMessage msg l@(RelicRoomSanctumOfFortune attrs) = runQueueT $ case msg of
    UseThisAbility iid this@(isProxySource attrs -> True) 1 -> do
      sid <- getRandom
      vaultDoor <- getJustLocationByName "Vault Door"
      chooseBeginSkillTest sid iid (toAbilitySource this 1) vaultDoor [#willpower, #agility] (Fixed 4)
      pure l
    PassedThisSkillTest iid (isProxyAbilitySource attrs 1 -> True) -> do
      vaultDoor <- getJustLocationByName "Vault Door"
      investigators <- select (investigatorAt vaultDoor)
      n <- getSpendableClueCount investigators
      x <- perPlayer 4
      when (n >= x) do
        chooseOneM iid $ scenarioI18n do
          labeled' "relicRoom.reveal" do
            spendCluesAsAGroup investigators x
            reveal attrs
          unscoped skip_
      pure l
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      wellspring <- selectJust $ scarletKeyIs Keys.theWellspringOfFortune
      clues <- fieldMap ScarletKeyTokens (countTokens #clue) wellspring
      moveTokens (attrs.ability 1) wellspring attrs #clue clues
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      wellspring <- selectJust $ scarletKeyIs Keys.theWellspringOfFortune
      push $ PlaceScarletKey wellspring (AttachedToInvestigator iid)
      pure l
    _ -> RelicRoomSanctumOfFortune <$> liftRunMessage msg attrs
