module Arkham.Location.Cards.CorriganIndustries (corriganIndustries) where

import Arkham.Ability
import Arkham.Card (toCard)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Token qualified as Token

newtype CorriganIndustries = CorriganIndustries LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corriganIndustries :: LocationCard CorriganIndustries
corriganIndustries = location CorriganIndustries Cards.corriganIndustries 2 (PerPlayer 1)

instance HasAbilities CorriganIndustries where
  getAbilities (CorriganIndustries a) =
    extendRevealed
      a
      [ restricted a 1 Here
          $ actionAbilityWithCost (SpendTokenCost Token.TimeCapsule (TargetIs $ toTarget a))
      , groupLimit PerGame
          $ restricted a 2 (Here <> Remembered ThomasAndMaryHaveMarried)
          $ actionAbilityWithCost (ActionCost 1)
      ]

instance RunMessage CorriganIndustries where
  runMessage msg l@(CorriganIndustries attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discards <- fieldMap InvestigatorDiscard (map toCard) iid
      focusCards discards do
        withI18n $ chooseUpToNM' iid 4 "doneChoosingCards" do
          for_ discards \card -> cardLabeled card $ addToHand iid (only card)
        unfocusCards
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      gainResources iid (attrs.ability 2) 10
      pure l
    _ -> CorriganIndustries <$> liftRunMessage msg attrs
