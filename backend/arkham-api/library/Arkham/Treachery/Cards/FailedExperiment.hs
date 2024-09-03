module Arkham.Treachery.Cards.FailedExperiment (failedExperiment, FailedExperiment (..)) where

import Arkham.Calculation
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FailedExperiment = FailedExperiment TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

failedExperiment :: TreacheryCard FailedExperiment
failedExperiment = treachery FailedExperiment Cards.failedExperiment

instance RunMessage FailedExperiment where
  runMessage msg t@(FailedExperiment attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs (SkillTestTarget sid)
        $ CalculatedDifficulty
        $ CountAssets
        $ assetControlledBy iid
        <> AssetWithAnyClues
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTestBy iid (isSource attrs -> True) _) | n > 0 -> do
      clues <- field InvestigatorClues iid
      mLocation <- field InvestigatorLocation iid
      if clues > 0 && isJust mLocation
        then chooseOneM iid do
          labeled "Take 1 horror" $ assignHorror iid attrs 1
          labeled "Place 1 of your clues on your location"
            $ push
            $ InvestigatorPlaceCluesOnLocation iid (toSource attrs) 1
        else assignHorror iid attrs 1
      doStep (n - 1) msg'
      pure t
    _ -> FailedExperiment <$> liftRunMessage msg attrs
