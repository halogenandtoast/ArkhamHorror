module Arkham.Location.Cards.OfficeMurderAtTheExcelsiorHotel (
  officeMurderAtTheExcelsiorHotel,
  OfficeMurderAtTheExcelsiorHotel (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Movement
import Arkham.Name
import Arkham.Projection

newtype OfficeMurderAtTheExcelsiorHotel = OfficeMurderAtTheExcelsiorHotel LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

officeMurderAtTheExcelsiorHotel :: LocationCard OfficeMurderAtTheExcelsiorHotel
officeMurderAtTheExcelsiorHotel = location OfficeMurderAtTheExcelsiorHotel Cards.officeMurderAtTheExcelsiorHotel 3 (PerPlayer 2)

instance HasModifiersFor OfficeMurderAtTheExcelsiorHotel where
  getModifiersFor (OfficeMurderAtTheExcelsiorHotel a) = whenUnrevealed a do
    modifySelect a (not_ $ HasMatchingAsset (assetIs Assets.managersKey)) [CannotEnter a.id]

instance HasAbilities OfficeMurderAtTheExcelsiorHotel where
  getAbilities (OfficeMurderAtTheExcelsiorHotel attrs) =
    if attrs.unrevealed
      then
        extend1 attrs
          $ skillTestAbility
          $ withTooltip
            "{action}: Test {agility} (3) to attempt to pick the lock. If you succeed, reveal Office and immediately move to it."
          $ restricted
            (proxied (LocationMatcherSource "Basement") attrs)
            1
            (OnLocation "Basement")
            #action
      else
        extend1
          attrs
          $ skillTestAbility
          $ withTooltip
            "{action}: Test {intellect} (0). For each point you succeed by, you may move 1 clue controlled by an investigator in the Office to Manager's key (if it is in play)."
          $ restricted attrs 1 Here actionAbility

instance RunMessage OfficeMurderAtTheExcelsiorHotel where
  runMessage msg l@(OfficeMurderAtTheExcelsiorHotel attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 0)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      iids <- selectWithField InvestigatorClues $ investigatorAt (toId attrs) <> InvestigatorWithAnyClues
      managersKey <- selectOne $ assetIs Assets.managersKey

      unless (null iids || isNothing managersKey) do
        named <- traverse (\(iid', x) -> (,x) <$> field InvestigatorName iid') iids
        chooseAmounts
          iid
          "number of clues to move to Alien Device"
          (MaxAmountTarget n)
          (map (\(name, x) -> (toTitle name, (0, x))) named)
          (toTarget attrs)
      pure l
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      named <- selectWithField InvestigatorName UneliminatedInvestigator
      let
        iidsWithAmounts =
          flip mapMaybe named $ \(iid', name) ->
            let amount = Msg.getChoiceAmount (toTitle name) choices
             in guard (amount > 0) $> (iid', amount)
      managersKey <- selectJust $ assetIs Assets.managersKey
      pushAll
        $ [ Msg.MovedClues (attrs.ability 1) (toSource iid) (toTarget managersKey) n
          | (iid, n) <- iidsWithAmounts
          ]
      pure l
    UseThisAbility iid p@(ProxySource _ (isSource attrs -> True)) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (toAbilitySource p 1) iid #agility (Fixed 3)
      pure l
    PassedThisSkillTest iid source@(AbilitySource (ProxySource _ (isSource attrs -> True)) 1) -> do
      pushAll [Msg.RevealLocation Nothing (toId attrs), Move $ move source iid (toId attrs)]
      pure l
    _ -> OfficeMurderAtTheExcelsiorHotel <$> liftRunMessage msg attrs
