module Arkham.Types.Asset.Cards.JazzMulligan
  ( jazzMulligan
  , JazzMulligan(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait

newtype JazzMulligan = JazzMulligan AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jazzMulligan :: AssetCard JazzMulligan
jazzMulligan =
  allyWith JazzMulligan Cards.jazzMulligan (2, 2) (isStoryL .~ True)

instance HasActions JazzMulligan where
  getActions (JazzMulligan x) = do
    [restrictedAbility x 1 (Unowned <> OnSameLocation) $ ActionAbility (Just Parley) $ ActionCost 1]

instance HasSet Trait env LocationId => HasModifiersFor env JazzMulligan where
  getModifiersFor (InvestigatorSource iid) (LocationTarget lid) (JazzMulligan attrs)
    | ownedBy attrs iid
    = do
      traits <- getSet lid
      pure [ toModifier attrs Blank | Miskatonic `member` traits ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env (), HasId LocationId env InvestigatorId) => RunMessage env JazzMulligan where
  runMessage msg a@(JazzMulligan attrs@AssetAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId iid
      a <$ push (AttachAsset assetId (LocationTarget lid))
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a
        <$ push
             (BeginSkillTest
               iid
               source
               (toTarget attrs)
               (Just Parley)
               SkillIntellect
               3
             )
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a <$ push (TakeControlOfAsset iid assetId)
    _ -> JazzMulligan <$> runMessage msg attrs
