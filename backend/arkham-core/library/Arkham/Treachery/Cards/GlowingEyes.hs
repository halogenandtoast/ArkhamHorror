module Arkham.Treachery.Cards.GlowingEyes (glowingEyes, GlowingEyes (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner hiding (treacheryInThreatAreaOf)

newtype GlowingEyes = GlowingEyes TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glowingEyes :: TreacheryCard GlowingEyes
glowingEyes = treachery GlowingEyes Cards.glowingEyes

instance HasAbilities GlowingEyes where
  getAbilities (GlowingEyes a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ forced
        $ RoundEnds #when
    ]

assetInThreatAreaOf :: InvestigatorId -> AssetMatcher
assetInThreatAreaOf = AssetWithPlacement . InThreatArea

instance RunMessage GlowingEyes where
  runMessage msg t@(GlowingEyes attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ enemyInThreatAreaOf iid
      assetCount <- selectCount $ assetInThreatAreaOf iid
      treacheryCount <-
        selectCount
          $ oneOf
          $ treacheryInThreatAreaOf iid
          : [TreacheryIsAttachedTo (toTarget enemy) | enemy <- enemies]
      let n = min 3 $ length enemies + assetCount + treacheryCount
      pushAll [assignHorror iid (attrs.ability 1) n, toDiscardBy iid (attrs.ability 1) attrs]
      pure t
    _ -> GlowingEyes <$> runMessage msg attrs
