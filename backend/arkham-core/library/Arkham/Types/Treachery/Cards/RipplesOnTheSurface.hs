module Arkham.Types.Treachery.Cards.RipplesOnTheSurface
  ( RipplesOnTheSurface(..)
  , ripplesOnTheSurface
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype RipplesOnTheSurface = RipplesOnTheSurface TreacheryAttrs
  deriving anyclass (IsTreachery, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ripplesOnTheSurface :: TreacheryCard RipplesOnTheSurface
ripplesOnTheSurface = treachery RipplesOnTheSurface Cards.ripplesOnTheSurface

instance
  ( HasId LocationId env InvestigatorId
  , HasSet Trait env LocationId
  )
  => HasModifiersFor env RipplesOnTheSurface where
  getModifiersFor (SkillTestSource _ _ source _ _) (InvestigatorTarget iid) (RipplesOnTheSurface attrs)
    | isSource attrs source
    = do
      locationId <- getId @LocationId iid
      isBayou <- member Bayou <$> getSet locationId
      pure $ toModifiers attrs [ CannotCommitCards | isBayou ]
  getModifiersFor _ _ _ = pure []

instance TreacheryRunner env => RunMessage env RipplesOnTheSurface where
  runMessage msg t@(RipplesOnTheSurface attrs@TreacheryAttrs {..}) =
    case msg of
      Revelation iid source | isSource attrs source -> t <$ pushAll
        [ RevelationSkillTest iid source SkillWillpower 3
        , Discard (TreacheryTarget treacheryId)
        ]
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
