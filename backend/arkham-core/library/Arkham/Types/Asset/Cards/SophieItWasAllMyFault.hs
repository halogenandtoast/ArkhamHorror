module Arkham.Types.Asset.Cards.SophieItWasAllMyFault
  ( sophieItWasAllMyFault
  , SophieItWasAllMyFault(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype SophieItWasAllMyFault = SophieItWasAllMyFault AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sophieItWasAllMyFault :: AssetCard SophieItWasAllMyFault
sophieItWasAllMyFault = assetWith
  SophieItWasAllMyFault
  Cards.sophieItWasAllMyFault
  (canLeavePlayByNormalMeansL .~ False)

instance HasActions SophieItWasAllMyFault where
  getActions (SophieItWasAllMyFault x) =
    [ restrictedAbility
          x
          1
          (OwnsThis <> InvestigatorExists
            (You <> InvestigatorWithDamage (AtMost $ Static 4))
          )
        $ ForcedAbility AnyWindow
    ]

instance HasModifiersFor env SophieItWasAllMyFault where
  getModifiersFor _ (InvestigatorTarget iid) (SophieItWasAllMyFault attrs)
    | ownedBy attrs iid = pure $ toModifiers attrs [AnySkillValue (-1)]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env SophieItWasAllMyFault where
  runMessage msg a@(SophieItWasAllMyFault attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (Flip (toSource attrs) (toTarget attrs))
    Flip _ target | isTarget attrs target -> do
      let
        sophieInLovingMemory = PlayerCard
          $ lookupPlayerCard Cards.sophieInLovingMemory (toCardId attrs)
        markId = fromJustNote "invalid" (assetInvestigator attrs)
      a <$ pushAll [ReplaceInvestigatorAsset markId sophieInLovingMemory]
    _ -> SophieItWasAllMyFault <$> runMessage msg attrs
