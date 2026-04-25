module Arkham.Act.Cards.EscapeTheDorms (escapeTheDorms) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Types (Field(..))
import Arkham.Helpers.Query (getJustLocationByName)
import Arkham.Projection
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype EscapeTheDorms = EscapeTheDorms ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapeTheDorms :: ActCard EscapeTheDorms
escapeTheDorms = act (2, A) EscapeTheDorms Cards.escapeTheDorms (groupClueCost $ PerPlayer 2)

instance HasAbilities EscapeTheDorms where
  getAbilities = actAbilities \a ->
    [ restricted a 1 (EachUndefeatedInvestigator $ at_ $ locationIs Locations.miskatonicQuad_c2026)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage EscapeTheDorms where
  runMessage msg a@(EscapeTheDorms attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- 1. Search all in-play and out-of-play Servant of Flame,
      --    heal all damage, and set them aside
      servants <- select $ EnemyWithTitle "Servant of Flame"
      for_ servants $ \eid -> do
        healAllDamage attrs eid
        card <- field EnemyCard eid
        removeEnemy eid
        setCardAside card

      -- 2. Discard each enemy in play
      discardEach attrs AnyEnemy

      -- 3. Discard all tokens and attachments from Your Friend's Room
      --    and remove it from the game
      yfroom <- getJustLocationByName "Your Friend's Room"
      push $ RemoveAllTokens (toSource attrs) (toTarget yfroom)
      push $ RemoveAllAttachments (toSource attrs) (toTarget yfroom)
      removeLocation yfroom

      -- 4. Put each remaining set-aside location into play
      placeSetAsideLocation_ Locations.orneLibrary_c2026
      placeSetAsideLocation_ Locations.scienceHall
      placeSetAsideLocation_ Locations.warrenObservatory_c2026

      advanceActDeck attrs
      pure a
    _ -> EscapeTheDorms <$> liftRunMessage msg attrs
