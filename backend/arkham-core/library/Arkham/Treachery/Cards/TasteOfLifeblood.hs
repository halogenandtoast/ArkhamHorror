module Arkham.Treachery.Cards.TasteOfLifeblood (tasteOfLifeblood, TasteOfLifeblood (..)) where

import Arkham.Helpers.Message qualified as Msg
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TasteOfLifeblood = TasteOfLifeblood TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tasteOfLifeblood :: TreacheryCard TasteOfLifeblood
tasteOfLifeblood = treachery TasteOfLifeblood Cards.tasteOfLifeblood

instance RunMessage TasteOfLifeblood where
  runMessage msg t@(TasteOfLifeblood attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      revelationSkillTest iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      push $ DoStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      clues <- field InvestigatorClues iid
      enemies <- select $ NearestEnemyTo iid AnyEnemy
      player <- getPlayer iid
      chooseOrRunOne iid
        $ [Label "Take 1 damage" [Msg.assignDamage iid attrs 1]]
        <> [ Label
            "Place 1 of your clues on your location"
            [InvestigatorPlaceCluesOnLocation iid (toSource attrs) 1]
           | clues > 0
           ]
        <> [ Label
            "Place 1 of your clues on nearest enemy"
            [ Msg.chooseOrRunOne
                player
                [targetLabel enemy [MovedClues (toSource iid) (toTarget enemy) 1] | enemy <- enemies]
            ]
           | clues > 0 && notNull enemies
           ]
      push $ DoStep (n - 1) msg'
      pure t
    _ -> TasteOfLifeblood <$> lift (runMessage msg attrs)
