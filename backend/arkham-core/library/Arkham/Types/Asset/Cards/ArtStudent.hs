module Arkham.Types.Asset.Cards.ArtStudent
  ( artStudent
  , ArtStudent(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

newtype ArtStudent = ArtStudent AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

artStudent :: AssetCard ArtStudent
artStudent = ally ArtStudent Cards.artStudent (1, 2)

instance HasAbilities env ArtStudent where
  getAbilities _ _ (ArtStudent x) = pure
    [ restrictedAbility
        x
        1
        OwnsThis
        (ReactionAbility
          (AssetEntersPlay Timing.When $ AssetWithId (toId x))
          Free
        )
    ]

instance
  ( HasSet InvestigatorId env ()
  , HasQueue env
  , HasModifiersFor env ()
  )
  => RunMessage env ArtStudent where
  runMessage msg a@(ArtStudent attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (InvestigatorDiscoverCluesAtTheirLocation iid 1 Nothing)
    _ -> ArtStudent <$> runMessage msg attrs
