module Arkham.Location.Cards.StrangeGeometry (strangeGeometry) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Label (mkLabel)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Name
import Control.Monad.Extra (findM)

newtype StrangeGeometry = StrangeGeometry LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeGeometry :: LocationCard StrangeGeometry
strangeGeometry = location StrangeGeometry Cards.strangeGeometry 4 (Static 1)

instance HasAbilities StrangeGeometry where
  getAbilities (StrangeGeometry a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ PhaseEnds #after #investigation
      , restricted a 2 (Here <> CluesOnThis (static 0) <> exists (RevealedLocation <> not_ (be a)))
          $ FastAbility Free
      ]

instance RunMessage StrangeGeometry where
  runMessage msg l@(StrangeGeometry attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      moveTo attrs iid (toId attrs)
      let labels = [nameToLabel (toName attrs) <> tshow @Int n | n <- [1 .. 2]]
      availableLabel <- findM (selectNone . LocationWithLabel . mkLabel) labels
      case availableLabel of
        Just label -> pure . StrangeGeometry $ attrs & labelL .~ label
        Nothing -> error "could not find label"
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      locationsWithMostClues <- select $ LocationWithMostClues $ not_ (be attrs)
      investigators <- select $ investigatorAt $ toId attrs
      enemies <- select $ UnengagedEnemy <> enemyAt (toId attrs)
      lead <- getLead
      let source = attrs.ability 1
      when (notNull investigators || notNull enemies) do
        chooseOrRunOneM lead do
          targets locationsWithMostClues \lid -> do
            for_ investigators \iid -> do
              moveTo (attrs.ability 1) iid lid
              assignDamageAndHorror iid source 1 1
            for_ enemies (`enemyMoveTo` lid)
      toDiscard attrs attrs
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      moveToMatch attrs iid $ RevealedLocation <> NotLocation (LocationWithId $ toId attrs)
      pure l
    _ -> StrangeGeometry <$> liftRunMessage msg attrs
