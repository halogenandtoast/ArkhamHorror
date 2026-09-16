module Arkham.Location.Cards.MurderAtTheExcelsiorHotel.Office (office) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetClues, AssetController))
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.CardDefs.MurderAtTheExcelsiorHotel qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Move
import Arkham.Name
import Arkham.Projection
import Arkham.Scenarios.MurderAtTheExcelsiorHotel.Helpers

newtype Office = Office LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

office :: LocationCard Office
office = location Office Cards.office 3 (PerPlayer 2)

instance HasModifiersFor Office where
  getModifiersFor (Office a) = whenUnrevealed a do
    modifySelect a (not_ $ HasMatchingAsset (assetIs Assets.managersKey)) [CannotEnter a.id]

instance HasAbilities Office where
  getAbilities (Office attrs) =
    scenarioI18n
      $ if attrs.unrevealed
        then
          extend1 attrs
            $ skillTestAbility
            $ withI18nTooltip "office.pickLock"
            $ restricted
              (proxied (LocationMatcherSource "Basement") attrs)
              1
              (OnLocation "Basement")
              #action
        else
          extend1
            attrs
            $ skillTestAbility
            $ withI18nTooltip "office.moveClues"
            $ restricted attrs 1 Here actionAbility

instance RunMessage Office where
  runMessage msg l@(Office attrs) = runQueueT $ scenarioI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 0)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      managersKey <- selectOne $ assetIs Assets.managersKey
      cluesOnKey <- maybe (pure 0) (field AssetClues) managersKey
      mcontroller <- join <$> traverse (field AssetController) managersKey

      iids <-
        selectWithField InvestigatorClues (investigatorAt (toId attrs) <> InvestigatorWithAnyClues) <&> mapMaybe \(iid', clues) -> do
          let total = if Just iid' == mcontroller then clues - cluesOnKey else clues
          guard (total > 0) $> (iid', max 0 total)

      unless (null iids || isNothing managersKey) do
        named <- traverse (\(iid', x) -> (,x) <$> field InvestigatorName iid') iids
        chooseAmounts
          iid
          (ikey' "label.office.moveClues")
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
      reveal attrs.id
      moveTo source iid attrs
      pure l
    _ -> Office <$> liftRunMessage msg attrs
