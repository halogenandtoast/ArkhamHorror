module Arkham.Treachery.Cards.RipplesOnTheSurface
  ( RipplesOnTheSurface(..)
  , ripplesOnTheSurface
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Trait
import qualified Arkham.Treachery.Cards as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype RipplesOnTheSurface = RipplesOnTheSurface TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ripplesOnTheSurface :: TreacheryCard RipplesOnTheSurface
ripplesOnTheSurface = treachery RipplesOnTheSurface Cards.ripplesOnTheSurface

instance HasModifiersFor RipplesOnTheSurface where
  getModifiersFor (SkillTestSource _ _ source _) (InvestigatorTarget iid) (RipplesOnTheSurface attrs)
    | isSource attrs source
    = do
      isBayou <- selectAny $ LocationWithTrait Bayou <> LocationWithInvestigator
        (InvestigatorWithId iid)
      pure $ toModifiers attrs [ CannotCommitCards AnyCard | isBayou ]
  getModifiersFor _ _ _ = pure []

instance RunMessage RipplesOnTheSurface where
  runMessage msg t@(RipplesOnTheSurface attrs@TreacheryAttrs {..}) =
    case msg of
      Revelation iid source | isSource attrs source ->
        t <$ push (RevelationSkillTest iid source SkillWillpower 3)
      FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
        | tid == treacheryId -> do
          push $ InvestigatorAssignDamage
            iid
            (TreacherySource treacheryId)
            DamageAny
            0
            n
          pure t
      _ -> RipplesOnTheSurface <$> runMessage msg attrs
