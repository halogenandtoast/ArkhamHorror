module Arkham.Types.Event.Cards.AstoundingRevelation
  ( astoundingRevelation
  , AstoundingRevelation(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Uses (UseType(..))
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype AstoundingRevelation = AstoundingRevelation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

astoundingRevelation :: EventCard AstoundingRevelation
astoundingRevelation = event AstoundingRevelation Cards.astoundingRevelation

ability :: InvestigatorId -> EventAttrs -> Ability
ability iid a = base
  { abilityLimit = PlayerLimit (PerSearch $ Just Research) 1
  }
 where
  base = mkAbility
    (toSource a)
    1
    (ReactionAbility (DiscardCost (SearchedCardTarget iid $ toCardId a)))

instance HasAbilities env AstoundingRevelation where
  getAbilities iid (WhenAmongSearchedCards who) (AstoundingRevelation attrs)
    | iid == who = pure [ability iid attrs]
  getAbilities _ _ _ = pure []

instance (Query AssetMatcher env, HasQueue env) => RunMessage env AstoundingRevelation where
  runMessage msg e@(AstoundingRevelation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      secretAssetIds <- selectList (AssetOwnedBy You <> AssetWithUseType Secret)
      e <$ push
        (chooseOne
          iid
          (TakeResources iid 2 False
          : [ AddUses (AssetTarget aid) Secret 1 | aid <- secretAssetIds ]
          )
        )
    _ -> AstoundingRevelation <$> runMessage msg attrs
