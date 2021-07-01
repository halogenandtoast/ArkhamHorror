module Arkham.Types.Asset.Cards.JazzMulligan
  ( jazzMulligan
  , JazzMulligan(..)
  )
where

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
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window

newtype JazzMulligan = JazzMulligan AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jazzMulligan :: AssetCard JazzMulligan
jazzMulligan = allyWith JazzMulligan Cards.jazzMulligan (2, 2) (isStoryL .~ True)

ability :: AssetAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ActionAbility (Just Parley) $ ActionCost 1)

instance HasId LocationId env InvestigatorId => HasActions env JazzMulligan where
  getActions iid NonFast (JazzMulligan attrs) = do
    lid <- getId iid
    case assetLocation attrs of
      Just location -> pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | lid == location && isNothing (assetInvestigator attrs)
        ]
      _ -> pure mempty
  getActions iid window (JazzMulligan attrs) = getActions iid window attrs

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
      a <$ unshiftMessage (AttachAsset assetId (LocationTarget lid))
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage
        (BeginSkillTest
          iid
          source
          (toTarget attrs)
          (Just Parley)
          SkillIntellect
          3
        )
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a
      <$ unshiftMessage (TakeControlOfAsset iid assetId)
    _ -> JazzMulligan <$> runMessage msg attrs
