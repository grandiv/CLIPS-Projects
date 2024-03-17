;;;===================================================================================
;;;     Developed by:
;;;     Muhammad Grandiv Lava Putra
;;;     22/493242/TK/54023
;;;     Teknologi Informasi
;;;
;;;     Forward Chaining Expert System to Recommend Outfit Based on User Preferences.
;;;
;;;     Developed on CLIPS Version 6.41 
;;;
;;;     To execute, just load, reset and run.
;;;===================================================================================


; Create templates
(deftemplate Outfit
   (slot nama)
   (slot gender)
   (slot cuaca)
   (slot occasion)
   (slot style)
   (slot budget)
)

(deftemplate User
   (slot nama)
   (slot gender-preference)
   (slot cuaca-preference)
   (slot occasion-preference)
   (slot style-preference)
   (slot budget)
)

; Create Rules
; Rule 1: Mengambil Input User
(defrule GetUserPreferences
    (not (User (nama ?nama)))
    =>
    ; inputs for user
    (printout t "Masukkan nama kamu! ")
    (bind ?nama (read))
    (printout t "Selamat datang, " ?nama ". Mari pilih outfit yang cocok bagimu!" crlf)
    (printout t "Apa gender kamu? (cowok/cewek) ")
    (bind ?gender-pref (read))
    (printout t "Bagaimana cuaca di tempatmu? (panas, dingin, hujan) ")
    (bind ?cuaca-pref (read))
    (printout t "Acara seperti apa yang ingin kamu datangi? (formal, santai, semi-formal) ")
    (bind ?occasion-pref (read))
    (printout t "Gaya apa yang kamu suka? (classic, casual, smart-casual, minimalis) ")
    (bind ?style-pref (read))
    (printout t "Berapa budget kamu? (rendah, sedang, tinggi) ")
    (bind ?budget (read))
    ; create new user fact
    (assert (User 
        (nama ?nama)
        (gender-preference ?gender-pref)
        (cuaca-preference ?cuaca-pref)
        (occasion-preference ?occasion-pref)
        (style-preference ?style-pref)
        (budget ?budget)
    ))
)

; Rule 2: Rekomendasi Outfit Berdasarkan Semua Kriteria
(defrule RecommendOutfitAllCriteria
    (User 
        (nama ?nama)
        (gender-preference ?gender-pref)
        (cuaca-preference ?cuaca-pref)
        (occasion-preference ?occasion-pref)
        (style-preference ?style-pref)
        (budget ?budget)
    )
    (Outfit 
        (nama ?outfit)
        (gender ?gender)
        (cuaca ?cuaca)
        (occasion ?occasion)
	(style ?style)
        (budget ?budget)
    )
    (test (eq ?gender-pref ?gender))
    (test (eq ?cuaca-pref ?cuaca))
    (test (eq ?occasion-pref ?occasion))
    (test (eq ?style-pref ?style))
    (test (eq ?budget ?budget))
    =>
    (printout t "Outfit paling cocok bagimu adalah " crlf)
    (printout t "==================================================================" crlf)
    (printout t ?outfit crlf)
    (printout t "=================================================================="crlf)
    (printout t "Gender: " ?gender crlf)
    (printout t "Cuaca: " ?cuaca crlf)
    (printout t "Occasion: " ?occasion crlf)
    (printout t "Style: " ?style crlf)
    (printout t "Budget: " ?budget crlf)
    (printout t "" crlf)
)

; Rule 3: Filter Berdasarkan Cuaca ('FilterByCuaca')
(defrule FilterByCuaca
    (User
        (nama ?nama)
        (cuaca-preference ?cuaca-pref)
        (gender-preference ?gender-pref)
    )
    (Outfit
        (nama ?outfit)
        (cuaca ?cuaca)
    )
    (test (eq ?cuaca-pref ?cuaca))
    =>
    (printout t "Berdasarkan cuacamu (" ?cuaca-pref "), kami merekomendasikan:" crlf)
    (printout t "Outfit: " ?outfit crlf)
    (printout t "" crlf)
)

; Rule 4: Filter Berdasarkan Budget ('FilterByBudget')
(defrule FilterByBudget
    (User
        (nama ?nama)
        (budget ?budget)
    )
    (Outfit
        (nama ?outfit)
        (budget ?budget)
    )
    (test (eq ?budget ?budget))
    =>
    (printout t "Berdasarkan budgetmu (" ?budget "), kami merekomendasikan:" crlf)
    (printout t "Outfit: " ?outfit crlf)
    (printout t "" crlf)
)

; Rule 5: Filter Berdasarkan Occasion ('FilterByOccasion')
(defrule FilterByOccasion
    (User
        (nama ?nama)
        (occasion-preference ?occasion-pref)
    )
    (Outfit
        (nama ?outfit)
        (occasion ?occasion)
    )
    (test (eq ?occasion-pref ?occasion))
    =>
    (printout t "Berdasarkan occasionmu (" ?occasion-pref "), kami merekomendasikan:" crlf)
    (printout t "Outfit: " ?outfit crlf)
    (printout t "" crlf)
)

; Rule 6: Filter Berdasarkan Gaya ('FilterByStyle')
(defrule FilterByStyle
    (User
        (nama ?nama)
        (style-preference ?style-pref)
    )
    (Outfit
        (nama ?outfit)
        (style ?style)
    )
    (test (eq ?style-pref ?style))
    =>
    (printout t "Berdasarkan gayamu (" ?style-pref "), kami merekomendasikan:" crlf)
    (printout t "Outfit: " ?outfit crlf)
    (printout t "" crlf)
)

; Rule 7: Filter Berdasarkan Gender ('FilterByGender')
(defrule FilterByGender
    (User
        (nama ?nama)
        (gender-preference ?gender-pref)
    )
    (Outfit
        (nama ?outfit)
        (gender ?gender)
    )
    (test (eq ?gender-pref ?gender))
    =>
    (printout t "Berdasarkan preferensi gendermu (" ?gender-pref "), kami merekomendasikan:" crlf)
    (printout t "Outfit: " ?outfit crlf)
    (printout t "" crlf)
)

; Rule 8: Filter Berdasarkan Kombinasi Gaya dan Cuaca ('FilterByStyleAndWeather')
(defrule FilterByStyleAndWeather
    (User
        (nama ?nama)
        (style-preference ?style-pref)
        (cuaca-preference ?cuaca-pref)
    )
    (Outfit
        (nama ?outfit)
        (style ?style)
        (cuaca ?cuaca)
    )
    (test (eq ?style-pref ?style))
    (test (eq ?cuaca-pref ?cuaca))
    =>
    (printout t "Berdasarkan gaya (" ?style-pref ") dan cuacamu (" ?cuaca-pref "), kami merekomendasikan:" crlf)
    (printout t "Outfit: " ?outfit crlf)
    (printout t "" crlf)
)

; Rule 9: NoOutfitRecommendation
(defrule NoOutfitRecommendation
    (declare (salience -4))
    (not (RecommendOutfitAllCriteria))
    =>
    (printout t crlf)
    (printout t "Maaf, kami tidak dapat merekomendasikan outfit berdasarkan preferensimu." crlf)
    (printout t "Tetap pantau, karena lebih banyak rekomendasi outfit akan segera hadir, atau coba preferensi lainnya :)" crlf)
)

;Rule 10: Exit Prompt
(defrule ExitRecommendation
   (User (nama ?nama))
   =>
   (printout t "Terima kasih telah menggunakan outfit recommender kami, " ?nama "!" crlf)
   ;(retract (User (nama ?nama)))
   ;(exit)
)

; set initial facts

(deffacts SampleOutfits
    (Outfit 
        (nama "Jas blazer hitam, celana ankle pants hitam, sepatu oxford cokelat") 
        (gender cowok) 
        (cuaca dingin)
        (occasion formal) 
        (style minimalis)
        (budget tinggi)
    )
    (Outfit 
        (nama "Dress putih, sandal gold") 
        (gender cewek) 
        (cuaca panas)
        (occasion santai) 
        (style casual)
        (budget sedang)
    )
    (Outfit 
        (nama "Kemeja flanel, jeans, sepatu sneakers") 
        (gender cowok) 
        (cuaca dingin)
        (occasion santai) 
        (style casual)
        (budget sedang)
    )
    (Outfit 
        (nama "Blouse floral, rok midi, high heels") 
        (gender cewek) 
        (cuaca panas)
        (occasion semi-formal) 
        (style smart-casual)
        (budget sedang)
    )
    (Outfit 
        (nama "Kaos polos, celana chino, sneakers") 
        (gender cowok) 
        (cuaca panas)
        (occasion santai) 
        (style casual)
        (budget rendah)
    )
    (Outfit 
        (nama "Gaun pesta merah, heels hitam") 
        (gender cewek) 
        (cuaca santai)
        (occasion formal) 
        (style classic)
        (budget tinggi)
    )
    (Outfit 
        (nama "Kemeja putih, celana panjang, loafer") 
        (gender cowok) 
        (cuaca panas)
        (occasion semi-formal) 
        (style smart-casual)
        (budget sedang)
    )
    (Outfit 
        (nama "Blazer abu-abu, kemeja biru, celana cokelat") 
        (gender cowok) 
        (cuaca dingin)
        (occasion formal) 
        (style minimalis)
        (budget tinggi)
    )
    (Outfit 
        (nama "Maxi dress warna pastel, sandal wedges") 
        (gender cewek) 
        (cuaca panas)
        (occasion santai) 
        (style casual)
        (budget sedang)
    )
    (Outfit 
        (nama "Kaos strip, celana jeans robek, sepatu sneakers") 
        (gender cowok) 
        (cuaca panas)
        (occasion santai) 
        (style casual)
        (budget rendah)
    )
    (Outfit 
        (nama "Blouse putih, rok plisket, ankle boots") 
        (gender cewek) 
        (cuaca dingin)
        (occasion semi-formal) 
        (style smart-casual)
        (budget sedang)
    )
    (Outfit 
        (nama "Jaket kulit, kaos hitam, jeans hitam, sepatu boot") 
        (gender cowok) 
        (cuaca dingin)
        (occasion santai) 
        (style casual)
        (budget sedang)
    )
)