module Arkham.Types.Asset.Cards.HaroldWalsted
  ( haroldWalsted
  , HaroldWalsted(..)
  ) where


import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Trait

newtype HaroldWalsted = HaroldWalsted AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

haroldWalsted :: AssetId -> HaroldWalsted
haroldWalsted uuid = HaroldWalsted $ (baseAttrs uuid "02138")
  { assetHealth = Just 1
  , assetSanity = Just 1
  , assetIsStory = True
  }

instance HasActions env HaroldWalsted where
  getActions iid window (HaroldWalsted attrs) = getActions iid window attrs

instance
  ( HasSet Trait env LocationId
  , HasId LocationId env InvestigatorId
  )
  => HasModifiersFor env HaroldWalsted where
  getModifiersFor (SkillTestSource _ _ _ (Just Action.Investigate)) (InvestigatorTarget iid) (HaroldWalsted attrs)
    = do
      lid <- getId @LocationId iid
      isMiskatonic <- member Miskatonic <$> getSet lid
      pure $ toModifiers
        attrs
        [ SkillModifier SkillIntellect 2 | isMiskatonic && ownedBy attrs iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env HaroldWalsted where
  runMessage msg a@(HaroldWalsted attrs) = case msg of
    Discard target | isTarget attrs target ->
      a <$ unshiftMessages [AddToken Tablet, RemoveFromGame target]
    _ -> HaroldWalsted <$> runMessage msg attrs
