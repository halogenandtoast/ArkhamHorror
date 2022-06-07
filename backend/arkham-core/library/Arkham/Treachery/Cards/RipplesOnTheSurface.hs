module Arkham.Treachery.Cards.RipplesOnTheSurface
  ( RipplesOnTheSurface(..)
  , ripplesOnTheSurface
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Runner
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype RipplesOnTheSurface = RipplesOnTheSurface TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ripplesOnTheSurface :: TreacheryCard RipplesOnTheSurface
ripplesOnTheSurface = treachery RipplesOnTheSurface Cards.ripplesOnTheSurface

instance
  ( HasId LocationId env InvestigatorId
  , HasSet Trait env LocationId
  )
  => HasModifiersFor RipplesOnTheSurface where
  getModifiersFor (SkillTestSource _ _ source _) (InvestigatorTarget iid) (RipplesOnTheSurface attrs)
    | isSource attrs source
    = do
      locationId <- getId @LocationId iid
      isBayou <- member Bayou <$> getSet locationId
      pure $ toModifiers attrs [ CannotCommitCards AnyCard | isBayou ]
  getModifiersFor _ _ _ = pure []

instance TreacheryRunner env => RunMessage RipplesOnTheSurface where
  runMessage msg t@(RipplesOnTheSurface attrs@TreacheryAttrs {..}) =
    case msg of
      Revelation iid source | isSource attrs source ->
        t <$ push (RevelationSkillTest iid source SkillWillpower 3)
      FailedSkillTest iid _ (TreacherySource tid) SkillTestInitiatorTarget{} _ n
        | tid == treacheryId
        -> t
          <$ push
               (InvestigatorAssignDamage
                 iid
                 (TreacherySource treacheryId)
                 DamageAny
                 0
                 n
               )
      _ -> RipplesOnTheSurface <$> runMessage msg attrs
