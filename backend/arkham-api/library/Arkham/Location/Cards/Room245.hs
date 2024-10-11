module Arkham.Location.Cards.Room245 (room245, Room245 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Modifiers ()
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (getChoiceAmount, pattern MovedClues)
import Arkham.Modifier
import Arkham.Name
import Arkham.Projection

newtype Room245 = Room245 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

room245 :: LocationCard Room245
room245 = locationWith Room245 Cards.room245 2 (PerPlayer 1) (labelL .~ "room245")

instance HasAbilities Room245 where
  getAbilities (Room245 a) = extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage Room245 where
  runMessage msg l@(Room245 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      fieldMap InvestigatorDiscard headMay iid >>= \case
        Just topOfDiscard -> do
          skillTestModifier sid (attrs.ability 1) topOfDiscard.id PlaceOnBottomOfDeckInsteadOfDiscard
          eachInvestigator \investigator -> do
            skillTestModifiers sid (attrs.ability 1) investigator
              $ if investigator == iid
                then
                  [ CanCommitToSkillTestsAsIfInHand $ toCard topOfDiscard
                  , CannotCommitCards (NotCard $ CardWithId $ toCardId topOfDiscard)
                  ]
                else [CannotCommitCards AnyCard]
        Nothing -> do
          eachInvestigator \investigator -> do
            skillTestModifiers sid (attrs.ability 1) investigator [CannotCommitCards AnyCard]

      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      iids <- selectWithField InvestigatorClues $ investigatorAt (toId attrs) <> InvestigatorWithAnyClues
      timeWornLocket <- selectOne $ assetIs Assets.timeWornLocket

      unless (null iids || isNothing timeWornLocket) $ do
        named <- traverse (\(iid', n) -> (,n) <$> field InvestigatorName iid') iids
        chooseAmounts
          iid
          "number of clues to move to Time-worn Locket"
          (MinAmountTarget 0)
          (map (\(name, n) -> (toTitle name, (0, n))) named)
          (toTarget attrs)
      pure l
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      named <- selectWithField InvestigatorName UneliminatedInvestigator
      let
        iidsWithAmounts =
          flip mapMaybe named $ \(iid', name) ->
            let amount = getChoiceAmount (toTitle name) choices
             in guard (amount > 0) $> (iid', amount)
      timeWornLocket <- selectJust $ assetIs Assets.timeWornLocket
      pushAll
        $ [ MovedClues (attrs.ability 1) (toSource iid) (toTarget timeWornLocket) n
          | (iid, n) <- iidsWithAmounts
          ]
      pure l
    _ -> Room245 <$> liftRunMessage msg attrs
