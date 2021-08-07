module Arkham.Types.Asset.Cards.SongOfTheDead2
  ( songOfTheDead2
  , SongOfTheDead2(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype SongOfTheDead2 = SongOfTheDead2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

songOfTheDead2 :: AssetCard SongOfTheDead2
songOfTheDead2 = arcaneWith
  SongOfTheDead2
  Cards.songOfTheDead2
  (startingUsesL ?~ Uses Charge 5)

instance HasActions env SongOfTheDead2 where
  getActions iid NonFast (SongOfTheDead2 a) = whenOwnedBy a iid
    $ pure [fightAction a 1 [ActionCost 1, UseCost (toId a) Charge 1]]
  getActions _ _ _ = pure []

instance HasModifiersFor env SongOfTheDead2

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env SongOfTheDead2 where
  runMessage msg a@(SongOfTheDead2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        source
        (InvestigatorTarget iid)
        (SkillModifier SkillWillpower 1)
      , CreateEffect "02112" Nothing source (InvestigatorTarget iid)
      , ChooseFightEnemy iid source SkillWillpower mempty False
      ]
    _ -> SongOfTheDead2 <$> runMessage msg attrs
