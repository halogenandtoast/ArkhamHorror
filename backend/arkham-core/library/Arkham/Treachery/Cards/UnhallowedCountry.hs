module Arkham.Treachery.Cards.UnhallowedCountry (
  UnhallowedCountry (..),
  unhallowedCountry,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection
import Arkham.SkillType
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype UnhallowedCountry = UnhallowedCountry TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unhallowedCountry :: TreacheryCard UnhallowedCountry
unhallowedCountry = treachery UnhallowedCountry Cards.unhallowedCountry

instance HasModifiersFor UnhallowedCountry where
  getModifiersFor (InvestigatorTarget iid) (UnhallowedCountry attrs) =
    pure
      $ toModifiers
        attrs
        [ CannotPlay (CardWithType AssetType <> CardWithTrait Ally)
        | treacheryOnInvestigator iid attrs
        ]
  getModifiersFor (AssetTarget aid) (UnhallowedCountry attrs) = do
    isAlly <- fieldMap AssetTraits (member Ally) aid
    miid <- selectAssetController aid
    pure $ case miid of
      Just iid ->
        toModifiers
          attrs
          [Blank | treacheryOnInvestigator iid attrs && isAlly]
      Nothing -> []
  getModifiersFor _ _ = pure []

instance HasAbilities UnhallowedCountry where
  getAbilities (UnhallowedCountry x) =
    [ restrictedAbility x 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ TurnEnds #when You
    ]

instance RunMessage UnhallowedCountry where
  runMessage msg t@(UnhallowedCountry attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      t <$ push (RevelationSkillTest iid source SkillWillpower (SkillTestDifficulty $ Fixed 3))
    PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> t <$ push (toDiscardBy iid (toAbilitySource attrs 1) $ toTarget attrs)
    _ -> UnhallowedCountry <$> runMessage msg attrs
