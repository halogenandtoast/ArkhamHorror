module Arkham.Treachery.Cards.RipplesOnTheSurface (
  RipplesOnTheSurface (..),
  ripplesOnTheSurface,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Source
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype RipplesOnTheSurface = RipplesOnTheSurface TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ripplesOnTheSurface :: TreacheryCard RipplesOnTheSurface
ripplesOnTheSurface = treachery RipplesOnTheSurface Cards.ripplesOnTheSurface

instance HasModifiersFor RipplesOnTheSurface where
  getModifiersFor (RipplesOnTheSurface attrs) = do
    getSkillTestSource >>= \case
      Just source | isSource attrs source -> do
        getSkillTestInvestigator >>= \case
          Nothing -> pure mempty
          Just iid -> maybeModified_ attrs iid do
            isBayou <- selectAny $ LocationWithTrait Bayou <> locationWithInvestigator iid
            guard isBayou
            pure [CannotCommitCards AnyCard]
      _ -> pure mempty

instance RunMessage RipplesOnTheSurface where
  runMessage msg t@(RipplesOnTheSurface attrs@TreacheryAttrs {..}) =
    case msg of
      Revelation iid source | isSource attrs source -> do
        sid <- getRandom
        push $ revelationSkillTest sid iid source #willpower (Fixed 3)
        pure t
      FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget {} _ n | tid == treacheryId -> do
        push $ InvestigatorAssignDamage iid (TreacherySource treacheryId) DamageAny 0 n
        pure t
      _ -> RipplesOnTheSurface <$> runMessage msg attrs
