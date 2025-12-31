module Arkham.Location.Cards.Room245 (room245) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isSkillTestSource)
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (pattern MovedClues)
import Arkham.Name
import Arkham.Projection

newtype Room245 = Room245 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

room245 :: LocationCard Room245
room245 = locationWith Room245 Cards.room245 2 (PerPlayer 1) (labelL .~ "room245")

instance HasAbilities Room245 where
  getAbilities (Room245 a) = extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance HasModifiersFor Room245 where
  getModifiersFor (Room245 attrs) = runMaybeT_ do
    liftGuardM $ isSkillTestSource (attrs.ability 1)
    iid <- MaybeT getSkillTestInvestigator
    mtop <- lift $ fieldMap InvestigatorDiscard headMay iid
    lift $ eachInvestigator \iid' -> do
      modified_ attrs iid'
        $ if
          | iid == iid'
          , Just topOfDiscard <- mtop ->
              [ CanCommitToSkillTestsAsIfInHand $ toCard topOfDiscard
              , CannotCommitCards (NotCard $ CardWithId $ toCardId topOfDiscard)
              ]
          | otherwise -> [CannotCommitCards AnyCard]
    for_ mtop \topOfDiscard -> modified_ attrs topOfDiscard [PlaceOnBottomOfDeckInsteadOfDiscard]

instance RunMessage Room245 where
  runMessage msg l@(Room245 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
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
