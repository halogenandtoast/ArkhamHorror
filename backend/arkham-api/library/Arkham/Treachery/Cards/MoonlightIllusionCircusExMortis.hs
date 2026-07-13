module Arkham.Treachery.Cards.MoonlightIllusionCircusExMortis (moonlightIllusionCircusExMortis) where

import Arkham.Campaigns.CircusExMortis.Helpers (getSealedMoonTokens)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose (chooseTargetM, chooseRevelationSkillTest)
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Projection
import Arkham.Trait (Trait (Woods))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MoonlightIllusion = MoonlightIllusion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlightIllusionCircusExMortis :: TreacheryCard MoonlightIllusion
moonlightIllusionCircusExMortis =
  treachery MoonlightIllusion Cards.moonlightIllusionCircusExMortis

instance RunMessage MoonlightIllusion where
  runMessage msg t@(MoonlightIllusion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      iids <- select (InvestigatorAt (locationWithInvestigator iid))
      extra <- sum . map length <$> traverse getSealedMoonTokens iids
      chooseRevelationSkillTest sid iid attrs [#willpower, #agility] (Fixed (3 + extra))
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      clues <- field InvestigatorClues iid
      when (clues > 0) $ placeCluesOnLocation iid attrs 1
      woods <- select (connectedTo (locationWithInvestigator iid) <> LocationWithTrait Woods)
      when (notNull woods) $ chooseTargetM iid woods \lid -> moveTo attrs iid lid
      pure t
    _ -> MoonlightIllusion <$> liftRunMessage msg attrs
