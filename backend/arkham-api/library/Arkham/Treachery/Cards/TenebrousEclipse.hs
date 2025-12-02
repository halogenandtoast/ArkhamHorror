module Arkham.Treachery.Cards.TenebrousEclipse (tenebrousEclipse) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Modifiers
import Arkham.Helpers.Modifiers (modified_)
import Arkham.Helpers.Source
import Arkham.Id
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TenebrousEclipse = TenebrousEclipse TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tenebrousEclipse :: TreacheryCard TenebrousEclipse
tenebrousEclipse = treachery TenebrousEclipse Cards.tenebrousEclipse

instance HasModifiersFor TenebrousEclipse where
  getModifiersFor (TenebrousEclipse a) = do
    let meta = toResultDefault @[InvestigatorId] [] a.meta
    for_ meta \iid -> modified_ a iid [CannotExpose]

instance HasAbilities TenebrousEclipse where
  getAbilities (TenebrousEclipse a) =
    [limited (MaxPer Cards.tenebrousEclipse PerRound 1) $ mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage TenebrousEclipse where
  runMessage msg t@(TenebrousEclipse attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    DoStep 1 (Flip iid source _) -> do
      byPlayer <- not <$> sourceMatches source SourceIsScenarioCardEffect
      if byPlayer
        then do
          let meta = toResultDefault [] attrs.meta
          pure $ TenebrousEclipse $ attrs & setMeta (iid : meta)
        else pure t
    EndRound -> do
      pure $ TenebrousEclipse $ attrs & setMeta @[InvestigatorId] []
    _ -> TenebrousEclipse <$> liftRunMessage msg attrs
