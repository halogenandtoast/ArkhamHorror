module Arkham.Treachery.Cards.TasteOfLifeblood (tasteOfLifeblood, TasteOfLifeblood (..)) where

import Arkham.Helpers.Investigator (canPlaceCluesOnYourLocation)
import Arkham.Helpers.Message qualified as Msg
import Arkham.I18n
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
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      push $ DoStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      clues <- field InvestigatorClues iid
      canPlaceClues <- canPlaceCluesOnYourLocation iid
      enemies <- select $ NearestEnemyToFallback iid AnyEnemy
      player <- getPlayer iid
      chooseOrRunOne iid
        $ [Label (withI18n $ countVar 1 $ ikey' "label.takeDamage") [Msg.assignDamage iid attrs 1]]
        <> [ Label
            (withI18n $ countVar 1 $ ikey' "label.placeCluesOnYourLocation")
            [InvestigatorPlaceCluesOnLocation iid (toSource attrs) 1]
           | canPlaceClues
           ]
        <> [ Label
            (withI18n $ countVar 1 $ ikey' "label.placeCluesOnNearestEnemy")
            [ Msg.chooseOrRunOne
                player
                [ targetLabel enemy [Msg.MovedClues (toSource attrs) (toSource iid) (toTarget enemy) 1]
                | enemy <- enemies
                ]
            ]
           | clues > 0 && notNull enemies
           ]
      push $ DoStep (n - 1) msg'
      pure t
    _ -> TasteOfLifeblood <$> liftRunMessage msg attrs
