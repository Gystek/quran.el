;;; quran.el --- Read the Holy Qur'an from Emacs -*- lexical-binding: t; -*-

;;; Copyright (C) Gustek 2023

;; Author: Gustek <gustek@riseup.net>
;; Created: 05 Aug 2023

;; Keywords: islam quran

;; This file is not part of GNU Emacs.

;;; Commentary: Depends on `curl' for downloading external files
;;; Code:

; Public

(defvar quran-buffers nil
  "Buffers opened by quran.el")

(defvar quran-buffer-zoom 5
  "The amount of zoom to pass to `text-scale-increase'")

(defvar quran-font "Amiri Quran"
  "The font for displaying the Arabic text of the Qur'an")

(defvar quran-type "uthmani"
  "The type of Qur'anic text to display.
   Can be \"simple\" or \"uthmani\"")

(defvar quran-pause-marks t
  "Whether or not to show the pause marks for recitation")

(defvar quran-sajdah-signs t
  "Whether or not to include sajdah (prostration) signs
   in the Qur'anic text")

(defvar quran-tatwil t
  "Whether or not to include tatwil (elongation) below
   superscript alifs")

(defvar quran-rub nil
  "Whether or not to include the rub-al-hizb (sign
   indicating the quarter of a hizb, or the eighth
   of a juz) in the Qur'anic text")

(defun quran-mode-enable ()
  (interactive)
  "Enable Qur'an mode for the current buffer"
  (if (not (member quran-type '("uthmani" "simple")))
      (error "Unknown `quran-type': %s" quran-type)
    (set-fontset-font "fontset-default" 'arabic quran-font)
    (text-scale-increase quran-buffer-zoom)
    (setq buffer-read-only t)
    (setq cursor-type nil)
    (toggle-truncate-lines -1)
    (visual-line-mode 1)))

(defun quran-mode-disable ()
  (interactive)
  "Disable Qur'an mode in the current buffer"
  (setq buffer-read-only nil)
  (setq cursor-type t)
  (text-scale-increase 0)
  (toggle-truncate-lines 1)
  (visual-line-mode -1))

(defun quran-read (ref)
  "Read a surah or ayat of the Qur'an.

  Reference syntax:
  - Surah:
    - <surah_number>
    - <surah_name>
  - Verse:
    - <verse_num>
    - <verse_num-verse_num>
  - Full reference:
    - <surah>[:<verse>[,<verse>]*]"

  (interactive "sReference: ")
  (pcase-let ((`(,surah . ,ayat) (__parse_reference ref)))
    (when (null __arabic-text-path)
      (let ((p (__download-arabic)))
	(while (eq (process-status p) 'run)
	  (sleep-for 0.1))))
    (__load-surah surah ayat)
    (quran-mode-enable)))

(defun quran-close ()
  "Kill all buffers opened by quran.el and delete text files"
  (interactive)
  (delete-file __arabic-text-path)
  (setq __arabic-text-path nil)
  (dolist (buf quran-buffers)
      (kill-buffer buf))
  (setf quran-buffers nil))

; Internals

(defun __number-to-arabic (n)
  (let ((numbers '("٠" "١" "٢" "٣" "٤" "٥" "٦" "٧" "٨" "٩"))
	(str ""))
    (while (> n 0)
      (setq str (concat (nth (% n 10) numbers) str))
      (setq n (/ n 10)))
    str))

(defvar __arabic-text-path nil)

(defun __make-download-link ()
  (format "https://tanzil.net/pub/download/index.php?%s%s%s%squranType=%s&outType=txt-2&agree=true"
	  (if quran-pause-marks "marks=true&" "")
	  (if quran-sajdah-signs "sajdah=true&" "")
	  (if quran-tatwil "tatweel=true&" "")
	  (if quran-rub "rub=true&" "")
	  quran-type))

(defun __download-arabic ()
  (setq __arabic-text-path (make-temp-file "arabic"))
  (save-window-excursion
    (make-process :name "Arabic download"
		  :command (list "curl"
				 (__make-download-link)
				 "-o"
				 __arabic-text-path)
		  :stderr (get-buffer "*Messages*"))))

(defun __expand-ayat (ayat)
  (let ((expanded '()))
    (dolist (el ayat)
      (let* ((split (string-split el "-"))
	     (start (string-to-number (car split)))
	     (end (if (> (length split) 1)
		      (string-to-number (nth 1 split))
		    nil)))
	(if (and end (not (= start end)))
	    (setq expanded (append expanded (number-sequence start end)))
	  (setq expanded (append expanded (list start))))))
    expanded))

(defun __parse_reference (ref)
  (let* ((split0 (string-split ref ":"))
	 (surah (car split0))
	 (surah-n (string-to-number surah)))
    (unless (> surah-n 0)
      (setq surah-n (1+ (cl-position surah __surahs-latin :test (lambda (name elem) (string= name (car elem)))))))
    (if (> (length split0) 1)
	(let ((ayat (string-split (nth 1 split0) ",")))
	  `(,surah-n . ,(__expand-ayat ayat)))
      `(,surah-n . nil))))

(defun __load-surah (num ayat)
  (cl-incf num -1)
  (let ((bname (format "Surah %s" (car (nth num __surahs-latin)))))
    (setq quran-buffers (cons bname quran-buffers))
    (switch-to-buffer bname))
  (quran-mode-disable)
  (erase-buffer)
  (insert-file-contents __arabic-text-path)

  ; Keep only this surah
  (let ((i 0))
    (while (< i num)
      (kill-line (cdr (nth i __surahs-latin)))
      (cl-incf i)))
  (forward-line (cdr (nth num __surahs-latin)))
  (set-mark-command nil)
  (end-of-buffer)
  (delete-region (mark) (point))

  ; Remove unwanted ayat

  (beginning-of-buffer)
  (unless (null ayat)
    (let ((length (cdr (nth num __surahs-latin)))
	  (i 0))
      (print ayat)
      (while (< i length)
	(cl-incf i)
	(if (member i ayat)
	    (progn
	      (insert (format "%s " (__number-to-arabic i)))
	      (goto-line (+ (line-number-at-pos) 1)))
	  (print i)
	  (kill-whole-line)))))

  ; Remove ayah and surah numbering
  (beginning-of-buffer)
  (replace-regexp "[0-9]+|" "")

  ; Remove the basmala if not in al-Fātiḥah
  (beginning-of-buffer)
  (unless (= num 0)
    (move-beginning-of-line nil)
    (set-mark-command nil)
    (move-end-of-line nil)
    (replace-string-in-region "بِسْمِ ٱللَّهِ ٱلرَّحْمَـٰنِ ٱلرَّحِيمِ "
			      ""
			      (mark)
			      (point))
    (move-beginning-of-line nil))

  (beginning-of-buffer))

(defvar __surahs-latin
  '(("al-Fātiḥah" . 7)
    ("al-Baqarah" . 286)
    ("ʾĀli ʿImrān" . 200)
    ("an-Nisāʾ" . 176)
    ("al-Māʾidah" . 120)
    ("al-ʾAnʿām" . 165)
    ("al-ʾAʿrāf" . 206)
    ("al-ʾAnfāl" . 75)
    ("at-Tawbah" . 129)
    ("Yūnus" . 109)
    ("Hūd" . 123)
    ("Yūsuf" . 111)
    ("ar-Raʿd" . 43)
    ("ʾIbrāhīm" . 52)
    ("al-Ḥijr" . 99)
    ("an-Naḥl" . 128)
    ("al-ʾIsrāʾ" . 111)
    ("al-Kahf" . 110)
    ("Maryam" . 98)
    ("Ṭāʾ Hāʾ" . 135)
    ("al-ʾAnbiyāʾ" . 112)
    ("al-Ḥajj" . 78)
    ("al-Muʾminūn" . 118)
    ("an-Nūr" . 64)
    ("al-Furqān" . 77)
    ("ash-Shuʿarāʾ" . 227)
    ("an-Naml" . 93)
    ("al-Qaṣaṣ" . 88)
    ("al-ʿAnkabūt" . 69)
    ("ar-Rūm" . 60)
    ("Luqmān" . 34)
    ("as-Sajdah" . 30)
    ("al-ʾAḥzāb" . 73)
    ("Sabaʾ" . 54)
    ("Fāṭir" . 45)
    ("Yāʾ Sīn" . 83)
    ("aṣ-Ṣāffāt" . 182)
    ("Ṣād" . 88)
    ("az-Zumar" . 75)
    ("Ghafir" . 85)
    ("Fuṣṣilat" . 54)
    ("ash-Shūrā" . 53)
    ("az-Zukhruf" . 89)
    ("ad-Dukhān" . 59)
    ("al-Jāthiyah" . 37)
    ("al-ʾAḥqāf" . 35)
    ("Muḥammad" . 38)
    ("al-Fatḥ" . 29)
    ("al-Ḥujurāt" . 18)
    ("Qāf" . 45)
    ("adh-Dhāriyāt" . 60)
    ("aṭ-Ṭūr" . 49)
    ("an-Najm" . 62)
    ("al-Qamar" . 55)
    ("ar-Raḥmān" . 78)
    ("al-Wāqiʿah" . 96)
    ("al-Ḥadīd" . 29)
    ("al-Mujādilah" . 22)
    ("al-Ḥashr" . 24)
    ("al-Mumtaḥanah" . 13)
    ("aṣ-Ṣaff" . 14)
    ("al-Jumuʿah" . 11)
    ("al-Munāfiqūn" . 11)
    ("at-Taghābun" . 18)
    ("aṭ-Ṭalāq" . 12)
    ("at-Taḥrīm" . 12)
    ("al-Mulk" . 30)
    ("al-Qalam" . 52)
    ("al-Ḥāqqah" . 52)
    ("al-Maʿārij" . 44)
    ("Nūḥ" . 28)
    ("al-Jinn" . 28)
    ("al-Muzzammil" . 20)
    ("al-Muddathir" . 56)
    ("al-Qiyamah" . 40)
    ("al-ʾInsān" . 31)
    ("al-Mursalāt" . 50)
    ("an-Nabaʾ" . 40)
    ("an-Nāziʿāt" . 46)
    ("ʿAbasa" . 42)
    ("at-Takwīr" . 29)
    ("al-ʾInfiṭār" . 19)
    ("al-Muṭaffifīn" . 36)
    ("al-ʾInshiqāq" . 25)
    ("al-Burūj" . 22)
    ("aṭ-Ṭāriq" . 17)
    ("al-ʾAʿlā" . 19)
    ("al-Ghāshyah" . 26)
    ("al-Fajr" . 30)
    ("al-Balad" . 20)
    ("ash-Shams" . 15)
    ("al-Layl" . 21)
    ("aḍ-Ḍuḥā" . 11)
    ("ash-Sharḥ" . 8)
    ("at-Tīn" . 8)
    ("al-ʿAlaq" . 19)
    ("al-Qadr" . 5)
    ("al-Bayyinah" . 8)
    ("az-Zalzalah" . 8)
    ("al-ʿĀdiyāt" . 11)
    ("al-Qāriʿah" . 11)
    ("at-Takāthur" . 8)
    ("al-ʿAṣr" . 3)
    ("al-Humazah" . 9)
    ("al-Fīl" . 5)
    ("Quraysh" . 4)
    ("al-Maʿūn" . 7)
    ("al-Kawthar" . 3)
    ("al-Kāfirūn" . 6)
    ("an-Naṣr" . 3)
    ("al-Masad" . 5)
    ("al-ʾIkhlāṣ" . 4)
    ("al-Falaq" . 5)
    ("an-Nās" . 6)))

;;; quran.el ends here
