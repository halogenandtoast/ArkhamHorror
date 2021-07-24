module Arkham.Types.Asset.Cards.JewelOfAureolus3
  ( jewelOfAureolus3
  , JewelOfAureolus3(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Token
import Arkham.Types.Window

newtype JewelOfAureolus3 = JewelOfAureolus3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jewelOfAureolus3 :: AssetCard JewelOfAureolus3
jewelOfAureolus3 = asset JewelOfAureolus3 Cards.jewelOfAureolus3

ability :: AssetAttrs -> Ability
ability attrs =
  mkAbility attrs 1 (ReactionAbility $ ExhaustCost (toTarget attrs))

instance HasActions env JewelOfAureolus3 where
  getActions iid (WhenRevealToken InvestigatorAtYourLocation token) (JewelOfAureolus3 x)
    | ownedBy x iid
    = pure
      [ UseAbility iid (ability x)
      | tokenFace token `elem` [Skull, Cultist, Tablet, ElderSign, AutoFail]
      ]
  getActions iid (WhenRevealToken You token) (JewelOfAureolus3 x)
    | ownedBy x iid = pure
      [ UseAbility iid (ability x)
      | tokenFace token `elem` [Skull, Cultist, Tablet, ElderSign, AutoFail]
      ]
  getActions iid window (JewelOfAureolus3 x) = getActions iid window x

instance HasModifiersFor env JewelOfAureolus3

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env JewelOfAureolus3 where
  runMessage msg a@(JewelOfAureolus3 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (chooseOne
        iid
        [ Label "Draw 1 Card" [DrawCards iid 1 False]
        , Label "Take 2 Resources" [TakeResources iid 2 False]
        ]
      )
    _ -> JewelOfAureolus3 <$> runMessage msg attrs
